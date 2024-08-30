;; BuyNsell: Enhanced Core Marketplace Smart Contract

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LISTING_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_LISTING_EXPIRED (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_INVALID_RATING (err u105))
(define-constant MAX_BATCH_SIZE u10)

;; Define data maps
(define-map Listings
  { listing-id: uint }
  {
    name: (string-ascii 50),
    description: (string-utf8 500),
    price: uint,
    seller: principal,
    category: (string-ascii 20),
    subcategory: (string-ascii 20),
    tags: (list 5 (string-ascii 20)),
    status: (string-ascii 10),
    created-at: uint,
    expires-at: uint
  }
)

(define-map UserBalances principal uint)
(define-map UserWishlists principal (list 100 uint))
(define-map SavedSearches principal (list 10 (string-ascii 100)))
(define-map ProductReviews { listing-id: uint, reviewer: principal } { rating: uint, review: (string-utf8 500) })

;; Define variables
(define-data-var last-listing-id uint u0)

;; Define functions

;; Create a new listing
(define-public (create-listing (name (string-ascii 50)) (description (string-utf8 500)) (price uint) 
                               (category (string-ascii 20)) (subcategory (string-ascii 20)) 
                               (tags (list 5 (string-ascii 20))) (duration uint))
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
(define-public (batch-create-listings (listings (list MAX_BATCH_SIZE {
                                        name: (string-ascii 50),
                                        description: (string-utf8 500),
                                        price: uint,
                                        category: (string-ascii 20),
                                        subcategory: (string-ascii 20),
                                        tags: (list 5 (string-ascii 20)),
                                        duration: uint
                                      })))
  (let
    ((results (map create-single-listing listings)))
    (ok results)
  )
)

;; Helper function for batch listing creation
(define-private (create-single-listing (listing {
                                         name: (string-ascii 50),
                                         description: (string-utf8 500),
                                         price: uint,
                                         category: (string-ascii 20),
                                         subcategory: (string-ascii 20),
                                         tags: (list 5 (string-ascii 20)),
                                         duration: uint
                                       }))
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
(define-public (update-listing-status (listing-id uint) (new-status (string-ascii 10)))
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
(define-public (buy-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_LISTING_NOT_FOUND))
      (buyer tx-sender)
      (seller (get seller listing))
      (price (get price listing))
    )
    (asserts! (is-eq (get status listing) "active") ERR_INVALID_STATUS)
    (asserts! (<= block-height (get expires-at listing)) ERR_LISTING_EXPIRED)
    (asserts! (>= (default-to u0 (map-get? UserBalances buyer)) price) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update balances
    (map-set UserBalances buyer (- (default-to u0 (map-get? UserBalances buyer)) price))
    (map-set UserBalances seller (+ (default-to u0 (map-get? UserBalances seller)) price))
    
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
    (ok (map-set UserWishlists tx-sender (unwrap! (as-max-len? (append current-wishlist listing-id) u100) ERR_NOT_AUTHORIZED)))
  )
)

;; Remove from wishlist
(define-public (remove-from-wishlist (listing-id uint))
  (let
    (
      (current-wishlist (default-to (list) (map-get? UserWishlists tx-sender)))
    )
    (ok (map-set UserWishlists tx-sender (filter (lambda (id) (not (is-eq id listing-id))) current-wishlist)))
  )
)

;; Get user's wishlist
(define-read-only (get-wishlist (user principal))
  (default-to (list) (map-get? UserWishlists user))
)

;; Save search criteria
(define-public (save-search (search-query (string-ascii 100)))
  (let
    (
      (current-searches (default-to (list) (map-get? SavedSearches tx-sender)))
    )
    (ok (map-set SavedSearches tx-sender (unwrap! (as-max-len? (append current-searches search-query) u10) ERR_NOT_AUTHORIZED)))
  )
)

;; Get user's saved searches
(define-read-only (get-saved-searches (user principal))
  (default-to (list) (map-get? SavedSearches user))
)

;; Add product review
(define-public (add-review (listing-id uint) (rating uint) (review (string-utf8 500)))
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

;; Search listings (simplified, actual implementation would be more complex and likely involve off-chain components)
(define-read-only (search-listings (keyword (string-ascii 20)) (category (optional (string-ascii 20))) (min-price (optional uint)) (max-price (optional uint)))
  (filter search-predicate (map-keys Listings))
)

;; Helper function for search
(define-private (search-predicate (listing { listing-id: uint }))
  (let
    (
      (listing-data (unwrap! (map-get? Listings listing) false))
    )
    (and
      (match keyword
        keyword (contains (get name listing-data) keyword)
        true
      )
      (match category
        category (is-eq (get category listing-data) category)
        true
      )
      (match min-price
        min-price (>= (get price listing-data) min-price)
        true
      )
      (match max-price
        max-price (<= (get price listing-data) max-price)
        true
      )
    )
  )
)

;; Get all active listings
(define-read-only (get-active-listings)
  (filter is-active-listing (map-keys Listings))
)

;; Helper function to check if a listing is active
(define-private (is-active-listing (listing { listing-id: uint }))
  (let ((listing-data (unwrap! (map-get? Listings listing) false)))
    (and
      (is-eq (get status listing-data) "active")
      (<= block-height (get expires-at listing-data))
    )
  )
)

;; Reminder for expiring listings (this would typically involve off-chain components)
(define-read-only (get-expiring-listings (days uint))
  (filter is-expiring-soon (map-keys Listings))
)

;; Helper function to check if a listing is expiring soon
(define-private (is-expiring-soon (listing { listing-id: uint }))
  (let 
    (
      (listing-data (unwrap! (map-get? Listings listing) false))
      (expiration-threshold (+ block-height (* days u144))) ;; Assuming 144 blocks per day
    )
    (and
      (is-eq (get status listing-data) "active")
      (<= (get expires-at listing-data) expiration-threshold)
    )
  )
)
