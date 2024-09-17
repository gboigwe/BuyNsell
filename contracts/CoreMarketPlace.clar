;; BuyNsell: Clarity Core Marketplace Smart Contract

(use-trait ft-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LISTING_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_LISTING_EXPIRED (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_INVALID_RATING (err u105))
(define-constant ERR_WISHLIST_FULL (err u106))
(define-constant ERR_SAVED_SEARCHES_FULL (err u107))
(define-constant ERR_DUPLICATE_WISHLIST_ITEM (err u108))
(define-constant ERR_NOT_BUYER (err u109))
(define-constant ERR_PRICE_ZERO (err u110))
(define-constant MAX_BATCH_SIZE u10)

;; Define a constant for the listing tuple type
(define-constant LISTING_TUPLE 
  {name: (string-utf8 64),
   description: (string-utf8 256),
   price: uint,
   category: (string-utf8 32),
   subcategory: (string-utf8 32),
   tags: (list 5 (string-utf8 32)),
   duration: uint})

;; Define data maps
(define-map Listings
  { listing-id: uint }
  {
    name: (string-utf8 64),
    description: (string-utf8 256),
    price: uint,
    seller: principal,
    buyer: (optional principal),
    category: (string-utf8 32),
    subcategory: (string-utf8 32),
    tags: (list 5 (string-utf8 32)),
    status: (string-utf8 16),
    created-at: uint,
    expires-at: uint
  }
)

(define-map Escrows
  { listing-id: uint }
  {
    buyer: principal,
    amount: uint,
    release-height: uint
  }
)

(define-map UserWishlists principal (list 100 uint))
(define-map SavedSearches principal (list 10 (string-utf8 256)))
(define-map ProductReviews { listing-id: uint, reviewer: principal } { rating: uint, review: (string-utf8 256) })

;; Define variables
(define-data-var last-listing-id uint u0)

;; Define events
(define-event listing-created (listing-id uint) (seller principal))
(define-event listing-updated (listing-id uint) (new-status (string-utf8 16)))
(define-event listing-purchased (listing-id uint) (buyer principal) (seller principal) (price uint))
(define-event escrow-created (listing-id uint) (buyer principal) (amount uint))
(define-event escrow-released (listing-id uint) (buyer principal) (seller principal) (amount uint))
(define-event review-added (listing-id uint) (reviewer principal) (rating uint))

;; Create a new listing
(define-public (create-listing (name (string-utf8 64)) (description (string-utf8 256)) (price uint) 
                               (category (string-utf8 32)) (subcategory (string-utf8 32)) 
                               (tags (list 5 (string-utf8 32))) (duration uint))
  (let
    (
      (listing-id (+ (var-get last-listing-id) u1))
      (expires-at (+ block-height duration))
    )
    (asserts! (> price u0) ERR_PRICE_ZERO)
    (map-set Listings
      { listing-id: listing-id }
      {
        name: name,
        description: description,
        price: price,
        seller: tx-sender,
        buyer: none,
        category: category,
        subcategory: subcategory,
        tags: tags,
        status: "active",
        created-at: block-height,
        expires-at: expires-at
      }
    )
    (var-set last-listing-id listing-id)
    (print (event-listing-created listing-id tx-sender))
    (ok listing-id)
  )
)

;; Batch create listings
(define-public (batch-create-listings (listings (list MAX_BATCH_SIZE LISTING_TUPLE)))
  (ok (map create-listing-from-tuple listings))
)

;; Helper function for batch listing creation
(define-private (create-listing-from-tuple (listing LISTING_TUPLE))
  (create-listing
    (get name listing)
    (get description listing)
    (get price listing)
    (get category listing)
    (get subcategory listing)
    (get tags listing)
    (get duration listing)
  )
)

;; Get listing details
(define-read-only (get-listing (listing-id uint))
  (map-get? Listings { listing-id: listing-id })
)

;; Update listing status
(define-public (update-listing-status (listing-id uint) (new-status (string-utf8 16)))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller listing)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq new-status "active") (is-eq new-status "cancelled")) ERR_INVALID_STATUS)
    (map-set Listings { listing-id: listing-id }
      (merge listing { status: new-status }))
    (print (event-listing-updated listing-id new-status))
    (ok true)
  )
)

;; Update listing price
(define-public (update-listing-price (listing-id uint) (new-price uint))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller listing)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-price u0) ERR_PRICE_ZERO)
    (ok (map-set Listings { listing-id: listing-id }
      (merge listing { price: new-price })))
  )
)

;; Buy a listing
(define-public (buy-listing (listing-id uint) (payment-token <ft-trait>))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (buyer tx-sender)
      (seller (get seller listing))
      (price (get price listing))
    )
    (asserts! (is-eq (get status listing) "active") ERR_INVALID_STATUS)
    (asserts! (<= block-height (get expires-at listing)) ERR_LISTING_EXPIRED)
    
    ;; Create escrow
    (try! (contract-call? payment-token transfer price buyer (as-contract tx-sender) none))
    (map-set Escrows { listing-id: listing-id }
      { buyer: buyer, amount: price, release-height: (+ block-height u144) }) ;; 24 hours in blocks
    
    ;; Update listing status and buyer
    (map-set Listings { listing-id: listing-id }
      (merge listing { status: "sold", buyer: (some buyer) }))
    
    (print (event-escrow-created listing-id buyer price))
    (print (event-listing-purchased listing-id buyer seller price))
    (ok true)
  )
)

;; Release escrow
(define-public (release-escrow (listing-id uint))
  (let
    (
      (escrow (unwrap! (map-get? Escrows { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (seller (get seller listing))
    )
    (asserts! (>= block-height (get release-height escrow)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (contract-call? 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard transfer 
      (get amount escrow) tx-sender seller none)))
    (map-delete Escrows { listing-id: listing-id })
    (print (event-escrow-released listing-id (get buyer escrow) seller (get amount escrow)))
    (ok true)
  )
)

;; Add to wishlist
(define-public (add-to-wishlist (listing-id uint))
  (let
    (
      (current-wishlist (default-to (list) (map-get? UserWishlists tx-sender)))
    )
    (asserts! (< (len current-wishlist) u100) ERR_WISHLIST_FULL)
    (asserts! (is-none (index-of current-wishlist listing-id)) ERR_DUPLICATE_WISHLIST_ITEM)
    (ok (map-set UserWishlists tx-sender (append current-wishlist listing-id)))
  )
)

;; Remove from wishlist
(define-public (remove-from-wishlist (listing-id uint))
  (let
    (
      (current-wishlist (default-to (list) (map-get? UserWishlists tx-sender)))
    )
    (ok (map-set UserWishlists tx-sender (filter not-matching-id current-wishlist)))
  )
)

;; Helper function for remove-from-wishlist
(define-private (not-matching-id (id uint))
  (not (is-eq id listing-id))
)

;; Get user's wishlist
(define-read-only (get-wishlist (user principal))
  (default-to (list) (map-get? UserWishlists user))
)

;; Save search criteria
(define-public (save-search (search-query (string-utf8 256)))
  (let
    (
      (current-searches (default-to (list) (map-get? SavedSearches tx-sender)))
    )
    (asserts! (< (len current-searches) u10) ERR_SAVED_SEARCHES_FULL)
    (ok (map-set SavedSearches tx-sender (append current-searches search-query)))
  )
)

;; Get user's saved searches
(define-read-only (get-saved-searches (user principal))
  (default-to (list) (map-get? SavedSearches user))
)

;; Add product review
(define-public (add-review (listing-id uint) (rating uint) (review (string-utf8 256)))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-eq (get status listing) "sold") ERR_INVALID_STATUS)
    (asserts! (is-eq (some tx-sender) (get buyer listing)) ERR_NOT_BUYER)
    (map-set ProductReviews { listing-id: listing-id, reviewer: tx-sender } { rating: rating, review: review })
    (print (event-review-added listing-id tx-sender rating))
    (ok true)
  )
)

;; Get product review
(define-read-only (get-review (listing-id uint) (reviewer principal))
  (map-get? ProductReviews { listing-id: listing-id, reviewer: reviewer })
)

;; Get last listing ID
(define-read-only (get-last-listing-id)
  (var-get last-listing-id)
)

;; Get listing IDs in a range
(define-read-only (get-listing-range (start uint) (end uint))
  (let
    (
      (last-id (var-get last-listing-id))
      (actual-end (if (> end last-id) last-id end))
    )
    (filter remove-none 
      (map get-listing-if-exists
        (create-range start actual-end)))
  )
)

;; Helper function to create a range of numbers
(define-private (create-range (start uint) (end uint))
  (fold add-to-range 
    (list)
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
  )
)

;; Helper function for create-range
(define-private (add-to-range (i uint) (acc (list 500 uint)))
  (let
    (
      (next (+ start i))
    )
    (if (<= next end)
      (unwrap-panic (as-max-len? (append acc next) u500))
      acc
    )
  )
)

;; Helper function to get listing if it exists
(define-private (get-listing-if-exists (id uint))
  (map-get? Listings { listing-id: id })
)

;; Helper function to remove None values
(define-private (remove-none (item (optional {
    name: (string-utf8 64),
    description: (string-utf8 256),
    price: uint,
    seller: principal,
    buyer: (optional principal),
    category: (string-utf8 32),
    subcategory: (string-utf8 32),
    tags: (list 5 (string-utf8 32)),
    status: (string-utf8 16),
    created-at: uint,
    expires-at: uint
  })))
  item
)

;; Initialize the contract
(begin
  (print "CoreMarketPlace contract deployed successfully")
)
