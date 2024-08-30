;; BuyNsell: Core Marketplace Smart Contract

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ITEM_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_LISTING_EXPIRED (err u103))
(define-constant ERR_INVALID_STATUS (err u104))

;; Define data maps
(define-map Listings
  { listing-id: uint }
  {
    name: (string-ascii 50),
    description: (string-utf8 500),
    price: uint,
    seller: principal,
    category: (string-ascii 20),
    status: (string-ascii 10),
    created-at: uint,
    expires-at: uint
  }
)

(define-map UserBalances principal uint)

;; Define variables
(define-data-var last-listing-id uint u0)

;; Define functions

;; Create a new listing
(define-public (create-listing (name (string-ascii 50)) (description (string-utf8 500)) (price uint) (category (string-ascii 20)) (duration uint))
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
        status: "active",
        created-at: block-height,
        expires-at: expires-at
      }
    )
    (var-set last-listing-id listing-id)
    (ok listing-id)
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
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_ITEM_NOT_FOUND))
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
      (listing (unwrap! (map-get? Listings { listing-id: listing-id }) ERR_ITEM_NOT_FOUND))
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

;; Deposit funds
(define-public (deposit (amount uint))
  (let ((new-balance (+ (default-to u0 (map-get? UserBalances tx-sender)) amount)))
    (map-set UserBalances tx-sender new-balance)
    (ok new-balance)
  )
)

;; Withdraw funds
(define-public (withdraw (amount uint))
  (let ((current-balance (default-to u0 (map-get? UserBalances tx-sender))))
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (map-set UserBalances tx-sender (- current-balance amount))
    (ok (- current-balance amount))
  )
)

;; Get user balance
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? UserBalances user))
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
