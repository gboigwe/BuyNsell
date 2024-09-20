;; BuyNsell Token (BST) Contract

;; Define token properties
(define-fungible-token bst u1000000000000)

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_AUTHORIZED (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_RECIPIENT (err u104))
(define-constant ERR_INVALID_URI (err u105))
(define-constant ERR_MAX_SUPPLY_REACHED (err u106))
(define-constant ERR_CONTRACT_PAUSED (err u107))
(define-constant MAX_SUPPLY u10000000000000) ;; 10 trillion tokens

;; Define variables
(define-data-var token-name (string-utf8 32) u"BuyNsell Token")
(define-data-var token-symbol (string-utf8 10) u"BST")
(define-data-var token-decimals uint u6)
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var contract-paused bool false)

;; Helper function to check if a principal is a valid recipient
(define-private (is-valid-recipient (recipient principal))
  (not (is-eq recipient (as-contract tx-sender))))

;; Helper function to check if the contract is not paused
(define-private (is-contract-not-paused)
  (not (var-get contract-paused)))

;; SIP-010: Transfer
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-contract-not-paused) ERR_CONTRACT_PAUSED)
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (ft-get-balance bst sender)) ERR_INSUFFICIENT_BALANCE)
        (asserts! (is-valid-recipient recipient) ERR_INVALID_RECIPIENT)
        (try! (ft-transfer? bst amount sender recipient))
        (print (merge 
            {event: "token_transferred", amount: amount, sender: sender, recipient: recipient}
            (match memo
                some-memo {memo: (some some-memo)}
                {memo: none}
            )
        ))
        (ok true)
    )
)

;; SIP-010: Get name
(define-read-only (get-name)
    (ok (var-get token-name))
)

;; SIP-010: Get symbol
(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

;; SIP-010: Get decimals
(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

;; SIP-010: Get balance
(define-read-only (get-balance (who principal))
    (ok (ft-get-balance bst who))
)

;; SIP-010: Get total supply
(define-read-only (get-total-supply)
    (ok (ft-get-supply bst))
)

;; SIP-010: Get token URI
(define-read-only (get-token-uri)
    (ok (var-get token-uri))
)

;; Mint new tokens (only contract owner)
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-contract-not-paused) ERR_CONTRACT_PAUSED)
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (is-valid-recipient recipient) ERR_INVALID_RECIPIENT)
        (asserts! (<= (+ amount (ft-get-supply bst)) MAX_SUPPLY) ERR_MAX_SUPPLY_REACHED)
        (match (ft-mint? bst amount recipient)
            success (begin
                (print {event: "token_minted", amount: amount, recipient: recipient})
                (ok success))
            error (err error))
    )
)

;; Burn tokens
(define-public (burn (amount uint) (sender principal))
    (begin
        (asserts! (is-contract-not-paused) ERR_CONTRACT_PAUSED)
        (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (ft-get-balance bst sender)) ERR_INSUFFICIENT_BALANCE)
        (match (ft-burn? bst amount sender)
            success (begin
                (print {event: "token_burned", amount: amount, sender: sender})
                (ok success))
            error (err error))
    )
)

;; Update token URI (only contract owner)
(define-public (set-token-uri (new-uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (match new-uri
            some-uri 
                (begin
                    (asserts! (<= (len some-uri) u256) ERR_INVALID_URI)
                    (var-set token-uri (some some-uri))
                    (print {event: "token_uri_updated", new_uri: some-uri})
                    (ok true)
                )
            (begin
                (var-set token-uri none)
                (print {event: "token_uri_removed"})
                (ok true)
            )
        )
    )
)

;; Pause contract (only contract owner)
(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (var-set contract-paused true)
        (print {event: "contract_paused"})
        (ok true)
    )
)

;; Unpause contract (only contract owner)
(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_OWNER_ONLY)
        (var-set contract-paused false)
        (print {event: "contract_unpaused"})
        (ok true)
    )
)

;; Initialize the contract
(begin
    ;; Mint initial supply to contract owner
    (try! (ft-mint? bst u1000000000000 CONTRACT_OWNER))
    ;; Print a deploy message
    (print {event: "contract_deployed", initial_supply: u1000000000000})
)
