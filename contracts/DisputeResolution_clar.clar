;; DisputeResolution: Handles disputes and arbitration in the BuyNsell marketplace

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_DISPUTE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATE (err u102))
(define-constant ERR_NOT_ARBITRATOR (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_VOTING_CLOSED (err u105))
(define-constant ERR_INSUFFICIENT_VOTES (err u106))
(define-constant ERR_INVALID_VOTE (err u107))
(define-constant ERR_NOT_INVOLVED_PARTY (err u108))
(define-constant ERR_INVALID_ESCROW_ID (err u109))
(define-constant ERR_INVALID_REASON (err u110))
(define-constant ERR_INVALID_DISPUTE_ID (err u111))
(define-constant ERR_INVALID_REWARD (err u112))
(define-constant ERR_INVALID_PRINCIPAL (err u113))
(define-constant VOTING_PERIOD u144)
(define-constant MIN_VOTES_REQUIRED u3)

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
    votes-for: uint,
    votes-against: uint,
    resolution: (optional (string-ascii 20))
  }
)

(define-map Arbitrators principal bool)
(define-map ArbitratorVotes { dispute-id: uint, arbitrator: principal } bool)

;; Variables
(define-data-var last-dispute-id uint u0)
(define-data-var arbitrator-reward uint u100) ;; Reward amount for voting

;; Private Functions
(define-private (is-arbitrator (user principal))
  (default-to false (map-get? Arbitrators user))
)

(define-private (has-voted (dispute-id uint) (arbitrator principal))
  (is-some (map-get? ArbitratorVotes { dispute-id: dispute-id, arbitrator: arbitrator }))
)

(define-private (update-vote-count (dispute-id uint) (vote bool))
  (match (map-get? Disputes { dispute-id: dispute-id })
    dispute (let
      (
        (new-votes-for (if vote (+ (get votes-for dispute) u1) (get votes-for dispute)))
        (new-votes-against (if vote (get votes-against dispute) (+ (get votes-against dispute) u1)))
      )
      (map-set Disputes { dispute-id: dispute-id }
        (merge dispute {
          votes-for: new-votes-for,
          votes-against: new-votes-against
        }))
      (ok true))
    (err ERR_DISPUTE_NOT_FOUND)
  )
)

(define-private (is-valid-escrow-id (escrow-id uint))
  (> escrow-id u0)
)

(define-private (is-valid-reason (reason (string-utf8 256)))
  (and (> (len reason) u0) (<= (len reason) u256))
)

(define-private (is-valid-dispute-id (dispute-id uint))
  (<= dispute-id (var-get last-dispute-id))
)

;; Public Functions
(define-public (raise-dispute (escrow-id uint) (counterparty principal) (reason (string-utf8 256)))
  (let
    (
      (dispute-id (+ (var-get last-dispute-id) u1))
    )
    (asserts! (is-valid-escrow-id escrow-id) (err ERR_INVALID_ESCROW_ID))
    (asserts! (is-valid-reason reason) (err ERR_INVALID_REASON))
    (asserts! (not (is-eq tx-sender counterparty)) (err ERR_NOT_INVOLVED_PARTY))
    (map-set Disputes
      { dispute-id: dispute-id }
      {
        escrow-id: escrow-id,
        initiator: tx-sender,
        counterparty: counterparty,
        reason: reason,
        status: "open",
        created-at: block-height,
        votes-for: u0,
        votes-against: u0,
        resolution: none
      }
    )
    (var-set last-dispute-id dispute-id)
    (print { event: "dispute_raised", dispute-id: dispute-id, escrow-id: escrow-id, initiator: tx-sender })
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote bool))
  (let
    (
      (dispute (unwrap! (map-get? Disputes { dispute-id: dispute-id }) (err ERR_DISPUTE_NOT_FOUND)))
    )
    (asserts! (is-valid-dispute-id dispute-id) (err ERR_INVALID_DISPUTE_ID))
    (asserts! (is-arbitrator tx-sender) (err ERR_NOT_ARBITRATOR))
    (asserts! (is-eq (get status dispute) "open") (err ERR_VOTING_CLOSED))
    (asserts! (<= (- block-height (get created-at dispute)) VOTING_PERIOD) (err ERR_VOTING_CLOSED))
    (asserts! (not (has-voted dispute-id tx-sender)) (err ERR_ALREADY_VOTED))
    (try! (update-vote-count dispute-id vote))
    (map-set ArbitratorVotes { dispute-id: dispute-id, arbitrator: tx-sender } vote)
    (print { event: "arbitrator_voted", dispute-id: dispute-id, arbitrator: tx-sender, vote: vote })
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? Disputes { dispute-id: dispute-id }) (err ERR_DISPUTE_NOT_FOUND)))
    )
    (asserts! (is-valid-dispute-id dispute-id) (err ERR_INVALID_DISPUTE_ID))
    (asserts! (is-eq (get status dispute) "open") (err ERR_INVALID_STATE))
    (asserts! (>= (+ (get votes-for dispute) (get votes-against dispute)) MIN_VOTES_REQUIRED) (err ERR_INSUFFICIENT_VOTES))
    (asserts! (<= (- block-height (get created-at dispute)) VOTING_PERIOD) (err ERR_VOTING_CLOSED))
    (let
      (
        (resolution (if (> (get votes-for dispute) (get votes-against dispute)) "for_initiator" "for_counterparty"))
      )
      (map-set Disputes { dispute-id: dispute-id }
        (merge dispute {
          status: "resolved",
          resolution: (some resolution)
        })
      )
      (print { event: "dispute_resolved", dispute-id: dispute-id, resolution: resolution })
      (ok resolution)
    )
  )
)

(define-public (add-arbitrator (arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-valid-principal arbitrator) (err ERR_INVALID_PRINCIPAL))
    (map-set Arbitrators arbitrator true)
    (print { event: "arbitrator_added", arbitrator: arbitrator })
    (ok true)
  )
)

(define-public (remove-arbitrator (arbitrator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (asserts! (is-valid-principal arbitrator) (err ERR_INVALID_PRINCIPAL))
    (map-delete Arbitrators arbitrator)
    (print { event: "arbitrator_removed", arbitrator: arbitrator })
    (ok true)
  )
)

(define-public (set-arbitrator-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_NOT_AUTHORIZED))
    (asserts! (> new-reward u0) (err ERR_INVALID_REWARD))
    (var-set arbitrator-reward new-reward)
    (print { event: "arbitrator_reward_set", new-reward: new-reward })
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-dispute (dispute-id uint))
  (map-get? Disputes { dispute-id: dispute-id })
)

(define-read-only (get-last-dispute-id)
  (ok (var-get last-dispute-id))
)

(define-read-only (get-arbitrator-reward)
  (ok (var-get arbitrator-reward))
)

(define-read-only (is-user-arbitrator (user principal))
  (ok (is-arbitrator user))
)

;; Helper function for principal validation
(define-private (is-valid-principal (principal principal))
  (and 
    (not (is-eq principal CONTRACT_OWNER))
    (not (is-eq principal (as-contract tx-sender)))
  )
)

;; Initialize contract
(begin
  (print "DisputeResolution contract initialized")
  (ok true)
)
