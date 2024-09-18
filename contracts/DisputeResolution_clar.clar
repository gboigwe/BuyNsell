;; DisputeResolution: Implements logic for handling disputes between users and manages arbitration processes

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_DISPUTE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_DISPUTE_STATUS (err u102))
(define-constant ERR_NOT_INVOLVED_PARTY (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_VOTING_CLOSED (err u105))
(define-constant ERR_INSUFFICIENT_VOTES (err u106))
(define-constant ERR_INVALID_VOTE (err u107))
(define-constant ARBITRATOR_COUNT u3)
(define-constant VOTING_PERIOD u144) ;; 24 hours in blocks (assuming 10-minute block times)

;; Data Maps
(define-map Disputes
  { dispute-id: uint }
  {
    escrow-id: uint,
    initiator: principal,
    counterparty: principal,
    reason: (string-utf8 256),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint),
    arbitrators: (list 3 principal),
    votes: (list 3 (optional bool)),
    resolution: (optional (string-ascii 20))
  }
)

(define-map Arbitrators principal bool)

;; Variables
(define-data-var last-dispute-id uint u0)

;; Private Functions
(define-private (is-arbitrator (user principal))
  (default-to false (map-get? Arbitrators user))
)

(define-private (get-random-arbitrators)
  (let
    (
      (arbitrator-list (unwrap! (contract-call? .random-number-generator get-random-items ARBITRATOR_COUNT) (list)))
    )
    arbitrator-list
  )
)

;; Public Functions
(define-public (raise-dispute (escrow-id uint) (reason (string-utf8 256)))
  (let
    (
      (dispute-id (+ (var-get last-dispute-id) u1))
      (escrow (unwrap! (contract-call? .escrow-service get-escrow escrow-id) (err ERR_DISPUTE_NOT_FOUND)))
      (buyer (get buyer escrow))
      (seller (get seller escrow))
    )
    (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) (err ERR_NOT_INVOLVED_PARTY))
    (asserts! (is-eq (get state escrow) "locked") (err ERR_INVALID_DISPUTE_STATUS))
    (let
      (
        (arbitrators (get-random-arbitrators))
      )
      (map-set Disputes
        { dispute-id: dispute-id }
        {
          escrow-id: escrow-id,
          initiator: tx-sender,
          counterparty: (if (is-eq tx-sender buyer) seller buyer),
          reason: reason,
          status: "open",
          created-at: block-height,
          resolved-at: none,
          arbitrators: arbitrators,
          votes: (list none none none),
          resolution: none
        }
      )
      (var-set last-dispute-id dispute-id)
      (print {event: "dispute_raised", dispute-id: dispute-id, escrow-id: escrow-id, initiator: tx-sender})
      (ok dispute-id)
    )
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote bool))
  (let
    (
      (dispute (unwrap! (map-get? Disputes { dispute-id: dispute-id }) (err ERR_DISPUTE_NOT_FOUND)))
      (arbitrator-index (index-of (get arbitrators dispute) tx-sender))
    )
    (asserts! (is-some arbitrator-index) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-eq (get status dispute) "open") (err ERR_VOTING_CLOSED))
    (asserts! (is-none (unwrap! (element-at (get votes dispute) (unwrap! arbitrator-index (err ERR_NOT_AUTHORIZED))) (err ERR_ALREADY_VOTED))) (err ERR_ALREADY_VOTED))
    (let
      (
        (updated-votes (replace-at? (get votes dispute) (unwrap! arbitrator-index (err ERR_NOT_AUTHORIZED)) (some vote)))
      )
      (map-set Disputes 
        { dispute-id: dispute-id }
        (merge dispute { votes: updated-votes })
      )
      (print {event: "arbitrator_voted", dispute-id: dispute-id, arbitrator: tx-sender, vote: vote})
      (ok true)
    )
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? Disputes { dispute-id: dispute-id }) (err ERR_DISPUTE_NOT_FOUND)))
    )
    (asserts! (is-eq (get status dispute) "open") (err ERR_INVALID_DISPUTE_STATUS))
    (asserts! (>= (len (filter is-some (get votes dispute))) u2) (err ERR_INSUFFICIENT_VOTES))
    (let
      (
        (positive-votes (len (filter id (map unwrap-panic (filter is-some (get votes dispute))))))
        (resolution (if (>= positive-votes u2) "buyer" "seller"))
      )
      (try! (contract-call? .escrow-service resolve-escrow (get escrow-id dispute) resolution))
      (map-set Disputes
        { dispute-id: dispute-id }
        (merge dispute 
          {
            status: "resolved",
            resolved-at: (some block-height),
            resolution: (some resolution)
          }
        )
      )
      (print {event: "dispute_resolved", dispute-id: dispute-id, resolution: resolution})
      (ok true)
    )
  )
)

(define-public (add-arbitrator (arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (ok (map-set Arbitrators arbitrator true))
  )
)

(define-public (remove-arbitrator (arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (ok (map-delete Arbitrators arbitrator))
  )
)

;; Read-only Functions
(define-read-only (get-dispute (dispute-id uint))
  (map-get? Disputes { dispute-id: dispute-id })
)

(define-read-only (get-last-dispute-id)
  (ok (var-get last-dispute-id))
)

;; Initialize contract
(begin
  (print "DisputeResolution contract initialized")
  (ok true)
)
