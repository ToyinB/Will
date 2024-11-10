;; Digital Will Smart Contract
;; Allows users to create and manage digital wills on the Stacks blockchain

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-BENEFICIARY (err u103))
(define-constant ERR-WILL-EXECUTED (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-PERIOD (err u106))
(define-constant ERR-ZERO-SHARE (err u107))
(define-constant ERR-INVALID-EXECUTOR (err u108))
(define-constant ERR-INVALID-ASSET-TYPE (err u109))
(define-constant ERR-INVALID-CUSTOM-DATA (err u110))
(define-constant ERR-INVALID-SHARE-VALUE (err u111))

;; Constants for validation
(define-constant VALID-ASSET-TYPES (list 
    "STX"
    "BTC"
    "NFT"
    "DIGITAL-ASSET"
    "PHYSICAL-ASSET"
    "OTHER"
))

(define-constant MAX-SHARE-VALUE u100)
(define-constant MIN-SHARE-VALUE u1)
(define-constant MAX-PERIOD-VALUE u31536000) ;; 365 days in seconds
(define-constant MIN-PERIOD-VALUE u2592000) ;; 30 days in seconds

;; Data Maps
(define-map wills
    principal
    {
        active: bool,
        executor: principal,
        last-modified: uint,
        executed: bool,
        total-shares: uint
    }
)

(define-map beneficiaries
    { will-owner: principal, beneficiary: principal }
    {
        share: uint,
        asset-type: (string-ascii 14),
        custom-data: (optional (string-utf8 256))
    }
)

(define-map proof-of-life
    principal
    {
        last-check-in: uint,
        check-in-period: uint
    }
)

;; Define data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var minimum-check-in-period uint MIN-PERIOD-VALUE)
(define-data-var maximum-check-in-period uint MAX-PERIOD-VALUE)

;; Private validation functions
(define-private (validate-custom-data (data (optional (string-utf8 256))))
    (match data
        custom-string (and 
            (is-some data)
            (< (len custom-string) u256)
            (> (len custom-string) u0)
            (not (is-eq custom-string u""))
        )
        true
    )
)

(define-private (validate-asset-type (asset-type (string-ascii 14)))
    (and
        (not (is-eq asset-type ""))
        (is-some (index-of VALID-ASSET-TYPES asset-type))
    )
)

(define-private (validate-share-value (share uint))
    (and 
        (>= share MIN-SHARE-VALUE)
        (<= share MAX-SHARE-VALUE)
    )
)

(define-private (validate-check-in-period (period uint))
    (and 
        (>= period MIN-PERIOD-VALUE)
        (<= period MAX-PERIOD-VALUE)
        (> period u0)
    )
)

(define-private (sanitize-string (input (string-utf8 256)))
    (let 
        (
            (default-value u"INVALID")
        )
        (if (> (len input) u0)
            input
            default-value
        )
    )
)

(define-private (sanitize-period (period uint))
    (if (and (>= period MIN-PERIOD-VALUE) (<= period MAX-PERIOD-VALUE))
        period
        MIN-PERIOD-VALUE
    )
)

(define-private (sanitize-share (share uint))
    (if (and (>= share MIN-SHARE-VALUE) (<= share MAX-SHARE-VALUE))
        share
        MIN-SHARE-VALUE
    )
)

;; Public functions

;; Initialize a new will
(define-public (create-will (executor principal))
    (let
        (
            (sender tx-sender)
        )
        (asserts! (not (is-eq executor sender)) ERR-INVALID-EXECUTOR)
        (asserts! (is-none (map-get? wills sender)) ERR-ALREADY-INITIALIZED)
        (ok (map-set wills
            sender
            {
                active: true,
                executor: executor,
                last-modified: block-height,
                executed: false,
                total-shares: u0
            }
        ))
    )
)

;; Add or update beneficiary with improved input validation
(define-public (set-beneficiary (beneficiary principal) (share uint) (asset-type (string-ascii 14)) (custom-data (optional (string-utf8 256))))
    (let
        (
            (sender tx-sender)
            (will-data (unwrap! (map-get? wills sender) ERR-NOT-FOUND))
            (current-beneficiary-data (map-get? beneficiaries { will-owner: sender, beneficiary: beneficiary }))
            (current-share (default-to u0 (get share current-beneficiary-data)))
            (sanitized-share (sanitize-share share))
            (is-valid-share (validate-share-value sanitized-share))
        )
        (asserts! (not (is-eq beneficiary sender)) ERR-INVALID-BENEFICIARY)
        (asserts! (not (get executed will-data)) ERR-WILL-EXECUTED)
        (asserts! is-valid-share ERR-ZERO-SHARE)
        (asserts! (validate-asset-type asset-type) ERR-INVALID-ASSET-TYPE)
        (asserts! (validate-custom-data custom-data) ERR-INVALID-CUSTOM-DATA)
        (asserts! (<= (- (+ (get total-shares will-data) sanitized-share) current-share) MAX-SHARE-VALUE) ERR-INVALID-SHARE-VALUE)
        
        ;; Update total shares in will
        (map-set wills
            sender
            (merge will-data { 
                total-shares: (- (+ (get total-shares will-data) sanitized-share) current-share), 
                last-modified: block-height 
            })
        )
        
        ;; Set beneficiary data
        (ok (map-set beneficiaries
            { will-owner: sender, beneficiary: beneficiary }
            {
                share: sanitized-share,
                asset-type: asset-type,
                custom-data: custom-data
            }
        ))
    )
)

;; Update proof of life with improved period validation
(define-public (check-in (optional-period (optional uint)))
    (let
        (
            (sender tx-sender)
            (current-time block-height)
            (raw-period (match optional-period
                period period
                (var-get minimum-check-in-period)
            ))
            (sanitized-period (sanitize-period raw-period))
        )
        (asserts! (validate-check-in-period sanitized-period) ERR-INVALID-PERIOD)
        
        (ok (map-set proof-of-life
            sender
            {
                last-check-in: current-time,
                check-in-period: sanitized-period
            }
        ))
    )
)

;; Execute will
(define-public (execute-will (will-owner principal))
    (let
        (
            (sender tx-sender)
            (will-data (unwrap! (map-get? wills will-owner) ERR-NOT-FOUND))
            (proof-data (unwrap! (map-get? proof-of-life will-owner) ERR-NOT-FOUND))
        )
        (asserts! (is-eq sender (get executor will-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active will-data) ERR-NOT-AUTHORIZED)
        (asserts! (not (get executed will-data)) ERR-WILL-EXECUTED)
        (asserts! (> block-height (+ (get last-check-in proof-data) (get check-in-period proof-data))) ERR-NOT-AUTHORIZED)
        (asserts! (> (get total-shares will-data) u0) ERR-INVALID-BENEFICIARY)
        
        (ok (map-set wills
            will-owner
            (merge will-data { 
                executed: true,
                active: false,
                last-modified: block-height 
            })
        ))
    )
)

;; Deactivate will
(define-public (deactivate-will)
    (let
        (
            (sender tx-sender)
            (will-data (unwrap! (map-get? wills sender) ERR-NOT-FOUND))
        )
        (asserts! (not (get executed will-data)) ERR-WILL-EXECUTED)
        (ok (map-set wills
            sender
            (merge will-data { 
                active: false,
                last-modified: block-height 
            })
        ))
    )
)

;; Update executor
(define-public (update-executor (new-executor principal))
    (let
        (
            (sender tx-sender)
            (will-data (unwrap! (map-get? wills sender) ERR-NOT-FOUND))
        )
        (asserts! (not (is-eq new-executor sender)) ERR-INVALID-EXECUTOR)
        (asserts! (not (get executed will-data)) ERR-WILL-EXECUTED)
        (ok (map-set wills
            sender
            (merge will-data { 
                executor: new-executor,
                last-modified: block-height 
            })
        ))
    )
)

;; Read-only functions

;; Get will details
(define-read-only (get-will (owner principal))
    (map-get? wills owner)
)

;; Get beneficiary details
(define-read-only (get-beneficiary (will-owner principal) (beneficiary principal))
    (map-get? beneficiaries { will-owner: will-owner, beneficiary: beneficiary })
)

;; Check if will is active
(define-read-only (is-will-active (owner principal))
    (match (map-get? wills owner)
        will-data (and (get active will-data) (not (get executed will-data)))
        false
    )
)

;; Get proof of life status
(define-read-only (get-proof-of-life-status (owner principal))
    (map-get? proof-of-life owner)
)

;; Get valid asset types
(define-read-only (get-valid-asset-types)
    VALID-ASSET-TYPES
)

;; Calculate time since last check-in
(define-private (time-since-last-check-in (owner principal))
    (match (map-get? proof-of-life owner)
        proof-data (- block-height (get last-check-in proof-data))
        u0
    )
)