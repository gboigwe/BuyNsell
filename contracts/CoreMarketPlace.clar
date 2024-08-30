;; Decentralized Marketplace Smart Contract

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ITEM_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))

;; Define data maps
(define-map Items
  { item-id: uint }
  { name: (string-ascii 50), price: uint, seller: principal, available: bool }
)

(define-map UserBalances principal uint)

;; Define functions

;; List a new item
(define-public (list-item (name (string-ascii 50)) (price uint))
  (let ((item-id (+ (default-to u0 (get-last-item-id)) u1)))
    (map-set Items { item-id: item-id }
      { name: name, price: price, seller: tx-sender, available: true })
    (ok item-id)
  )
)

;; Get the last item ID (helper function)
(define-read-only (get-last-item-id)
  (default-to u0 (element-at (map-keys Items) (- (len (map-keys Items)) u1)))
)

;; Get item details
(define-read-only (get-item (item-id uint))
  (map-get? Items { item-id: item-id })
)

;; Buy an item
(define-public (buy-item (item-id uint))
  (let (
    (item (unwrap! (map-get? Items { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    (buyer tx-sender)
    (seller (get seller item))
    (price (get price item))
  )
    (asserts! (get available item) ERR_ITEM_NOT_FOUND)
    (asserts! (>= (default-to u0 (map-get? UserBalances buyer)) price) ERR_INSUFFICIENT_BALANCE)
    
    ;; Update balances
    (map-set UserBalances buyer (- (default-to u0 (map-get? UserBalances buyer)) price))
    (map-set UserBalances seller (+ (default-to u0 (map-get? UserBalances seller)) price))
    
    ;; Mark item as unavailable
    (map-set Items { item-id: item-id }
      (merge item { available: false }))
    
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
