;; CoreMarketPlace: Manages listings and purchases for the BuyNsell decentralized marketplace

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LISTING_NOT_FOUND (err u101))
(define-constant ERR_INVALID_PRICE (err u102))
(define-constant ERR_INVALID_SELLER (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_LISTING_EXPIRED (err u105))
(define-constant ERR_INVALID_STATUS (err u106))
(define-constant ERR_NOT_SELLER (err u107))
(define-constant ERR_ALREADY_PURCHASED (err u108))
(define-constant ERR_INVALID_INPUT (err u109))
(define-constant ERR_INVALID_DURATION (err u110))
(define-constant ERR_INVALID_LISTING_ID (err u111))
(define-constant MAX_LISTING_DURATION u52560) ;; Max duration of 1 year (assuming 10-minute block times)

;; Data Maps
(define-map Listings
  { listing-id: uint }
  {
    seller: principal,
    name: (string-utf8 64),
    description: (string-utf8 256),
    price: uint,
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint
  }
)

;; Variables
(define-data-var last-listing-id uint u0)

;; Private Functions
(define-private (is-valid-price (price uint))
  (> price u0)
)

(define-private (is-valid-seller (seller principal))
  (and 
    (not (is-eq seller tx-sender))
    (not (is-eq seller (as-contract tx-sender)))
  )
)

(define-private (is-valid-string (str (string-utf8 256)) (max-len uint))
  (and (>= (len str) u1) (<= (len str) max-len))
)

(define-private (is-valid-duration (duration uint))
  (and (> duration u0) (<= duration MAX_LISTING_DURATION))
)

(define-private (is-valid-listing-id (id uint))
  (<= id (var-get last-listing-id))
)

(define-private (increment-listing-id)
  (let 
    (
      (current-id (var-get last-listing-id))
    )
    (var-set last-listing-id (+ current-id u1))
    (var-get last-listing-id)
  )
)

;; Public Functions
(define-public (create-listing (name (string-utf8 64)) (description (string-utf8 256)) (price uint) (duration uint))
  (let
    (
      (listing-id (increment-listing-id))
      (expires-at (+ block-height duration))
    )
    (asserts! (is-valid-price price) (err ERR_INVALID_PRICE))
    (asserts! (is-valid-string name u64) (err ERR_INVALID_INPUT))
    (asserts! (is-valid-string description u256) (err ERR_INVALID_INPUT))
    (asserts! (is-valid-duration duration) (err ERR_INVALID_DURATION))
    (map-set Listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        name: name,
        description: description,
        price: price,
        status: "active",
        created-at: block-height,
        expires-at: expires-at
      }
    )
    (print { event: "listing_created", listing-id: listing-id, seller: tx-sender })
    (ok listing-id)
  )
)

(define-public (update-listing (listing-id uint) (new-price uint) (new-description (string-utf8 256)))
  (begin
    (asserts! (is-valid-listing-id listing-id) (err ERR_INVALID_LISTING_ID))
    (let
      (
        (listing (unwrap! (map-get? Listings { listing-id: listing-id }) (err ERR_LISTING_NOT_FOUND)))
      )
      (asserts! (is-eq (get seller listing) tx-sender) (err ERR_NOT_SELLER))
      (asserts! (is-eq (get status listing) "active") (err ERR_INVALID_STATUS))
      (asserts! (is-valid-price new-price) (err ERR_INVALID_PRICE))
      (asserts! (is-valid-string new-description u256) (err ERR_INVALID_INPUT))
      (map-set Listings
        { listing-id: listing-id }
        (merge listing 
          {
            price: new-price,
            description: new-description
          }
        )
      )
      (print { event: "listing_updated", listing-id: listing-id, seller: tx-sender })
      (ok true)
    )
  )
)

(define-public (cancel-listing (listing-id uint))
  (begin
    (asserts! (is-valid-listing-id listing-id) (err ERR_INVALID_LISTING_ID))
    (let
      (
        (listing (unwrap! (map-get? Listings { listing-id: listing-id }) (err ERR_LISTING_NOT_FOUND)))
      )
      (asserts! (is-eq (get seller listing) tx-sender) (err ERR_NOT_SELLER))
      (asserts! (is-eq (get status listing) "active") (err ERR_INVALID_STATUS))
      (map-set Listings
        { listing-id: listing-id }
        (merge listing { status: "cancelled" })
      )
      (print { event: "listing_cancelled", listing-id: listing-id, seller: tx-sender })
      (ok true)
    )
  )
)

(define-public (purchase-listing (listing-id uint))
  (begin
    (asserts! (is-valid-listing-id listing-id) (err ERR_INVALID_LISTING_ID))
    (let
      (
        (listing (unwrap! (map-get? Listings { listing-id: listing-id }) (err ERR_LISTING_NOT_FOUND)))
        (price (get price listing))
        (seller (get seller listing))
      )
      (asserts! (is-eq (get status listing) "active") (err ERR_INVALID_STATUS))
      (asserts! (<= block-height (get expires-at listing)) (err ERR_LISTING_EXPIRED))
      (asserts! (is-valid-seller seller) (err ERR_INVALID_SELLER))
      (match (stx-transfer? price tx-sender seller)
        success (begin
          (map-set Listings
            { listing-id: listing-id }
            (merge listing { status: "sold" })
          )
          (print { event: "listing_purchased", listing-id: listing-id, buyer: tx-sender, seller: seller, price: price })
          (ok true))
        error (err ERR_INSUFFICIENT_BALANCE))
    )
  )
)

;; Read-only Functions
(define-read-only (get-listing (listing-id uint))
  (if (is-valid-listing-id listing-id)
    (map-get? Listings { listing-id: listing-id })
    none
  )
)

(define-read-only (get-last-listing-id)
  (ok (var-get last-listing-id))
)

;; Initialize contract
(begin
  (print "CoreMarketPlace contract initialized")
  (ok true)
)
