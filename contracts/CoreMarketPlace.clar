;; BuyNsell: Clarity 3.0 Core Marketplace Smart Contract

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
(define-constant MAX_BATCH_SIZE u10)

;; Define a constant for the listing tuple type
(define-constant LISTING_TUPLE 
  {name: (string-ascii 64),
   description: (string-ascii 256),
   price: uint,
   category: (string-ascii 32),
   subcategory: (string-ascii 32),
   tags: (list 5 (string-ascii 32)),
   duration: uint})

;; Define data maps
(define-map Listings
  { listing-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    price: uint,
    seller: principal,
    category: (string-ascii 32),
    subcategory: (string-ascii 32),
    tags: (list 5 (string-ascii 32)),
    status: (string-ascii 16),
    created-at: uint,
    expires-at: uint
  }
)

(define-map UserBalances principal uint)
(define-map UserWishlists principal (list 100 uint))
(define-map SavedSearches principal (list 10 (string-ascii 256)))
(define-map ProductReviews { listing-id: uint, reviewer: principal } { rating: uint, review: (string-ascii 256) })

;; Define variables
(define-data-var last-listing-id uint u0)

;; Create a new listing
(define-public (create-listing (name (string-ascii 64)) (description (string-ascii 256)) (price uint) 
                               (category (string-ascii 32)) (subcategory (string-ascii 32)) 
                               (tags (list 5 (string-ascii 32))) (duration uint))
  (let
    (
      (listing-id (+ (var-get last-listing-id) u1))
      (expires-at (+ block-height duration))
    )
    (map-set Listings
      { listing-id: listing-id }
      {
        name: name,
        description: description,
        price: price,
        seller: tx-sender,
        category: category,
        subcategory: subcategory,
        tags: tags,
        status: "active",
        created-at: block-height,
        expires-at: expires-at
      }
    )
    (var-set last-listing-id listing-id)
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
(define-public (update-listing-status (listing-id uint) (new-status (string-ascii 16)))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller listing)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq new-status "active") (is-eq new-status "cancelled")) ERR_INVALID_STATUS)
    (ok (map-set Listings { listing-id: listing-id }
      (merge listing { status: new-status })))
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
    
    ;; Transfer payment
    (try! (contract-call? payment-token transfer price buyer seller none))
    
    ;; Update listing status
    (map-set Listings { listing-id: listing-id }
      (merge listing { status: "sold" }))
    
    (ok true)
  )
)

;; Add to wishlist
(define-public (add-to-wishlist (listing-id uint))
  (let
    (
      (current-wishlist (default-to (list) (map-get? UserWishlists tx-sender)))
    )
    (if (< (len current-wishlist) u100)
        (ok (map-set UserWishlists tx-sender (append current-wishlist listing-id)))
        ERR_WISHLIST_FULL)
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
(define-public (save-search (search-query (string-ascii 256)))
  (let
    (
      (current-searches (default-to (list) (map-get? SavedSearches tx-sender)))
    )
    (if (< (len current-searches) u10)
        (ok (map-set SavedSearches tx-sender (append current-searches search-query)))
        ERR_SAVED_SEARCHES_FULL)
  )
)

;; Get user's saved searches
(define-read-only (get-saved-searches (user principal))
  (default-to (list) (map-get? SavedSearches user))
)

;; Add product review
(define-public (add-review (listing-id uint) (rating uint) (review (string-ascii 256)))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-eq (get status listing) "sold") ERR_INVALID_STATUS)
    (ok (map-set ProductReviews { listing-id: listing-id, reviewer: tx-sender } { rating: rating, review: review }))
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
    name: (string-ascii 64),
    description: (string-ascii 256),
    price: uint,
    seller: principal,
    category: (string-ascii 32),
    subcategory: (string-ascii 32),
    tags: (list 5 (string-ascii 32)),
    status: (string-ascii 16),
    created-at: uint,
    expires-at: uint
  })))
  item
)
