;; FluxonZen Prediction Market Protocol
;; AI-powered prediction markets

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-market-closed (err u104))
(define-constant err-market-resolved (err u105))
(define-constant err-invalid-outcome (err u106))
(define-constant err-unauthorized (err u107))

;; Data Variables
(define-data-var market-nonce uint u0)
(define-data-var total-flux-supply uint u1000000000000) ;; 1M FLUX tokens
(define-data-var protocol-fee-rate uint u25) ;; 0.25% fee (25 basis points)

;; Data Maps
(define-map markets
    { market-id: uint }
    {
        creator: principal,
        question: (string-ascii 256),
        end-time: uint,
        resolved: bool,
        winning-outcome: (optional uint),
        total-liquidity: uint,
        confidence-score: uint ;; 0-100
    }
)

(define-map market-outcomes
    { market-id: uint, outcome-id: uint }
    {
        description: (string-ascii 128),
        total-staked: uint,
        liquidity-depth: uint
    }
)

(define-map user-predictions
    { user: principal, market-id: uint, outcome-id: uint }
    {
        amount: uint,
        timestamp: uint
    }
)

(define-map flux-balances
    { user: principal }
    { balance: uint }
)

(define-map zen-balances
    { user: principal }
    { balance: uint }
)

(define-map bond-balances
    { user: principal }
    { balance: uint, locked-until: uint }
)

(define-map liquidity-providers
    { user: principal, market-id: uint }
    { provided: uint, shares: uint }
)

(define-map oracles
    { oracle: principal }
    { reputation: uint, total-resolutions: uint }
)

;; Read-only functions
(define-read-only (get-market (market-id uint))
    (map-get? markets { market-id: market-id })
)

(define-read-only (get-market-outcome (market-id uint) (outcome-id uint))
    (map-get? market-outcomes { market-id: market-id, outcome-id: outcome-id })
)

(define-read-only (get-user-prediction (user principal) (market-id uint) (outcome-id uint))
    (map-get? user-predictions { user: user, market-id: market-id, outcome-id: outcome-id })
)

(define-read-only (get-flux-balance (user principal))
    (default-to { balance: u0 } (map-get? flux-balances { user: user }))
)

(define-read-only (get-zen-balance (user principal))
    (default-to { balance: u0 } (map-get? zen-balances { user: user }))
)

(define-read-only (get-bond-balance (user principal))
    (default-to { balance: u0, locked-until: u0 } (map-get? bond-balances { user: user }))
)

(define-read-only (get-protocol-fee-rate)
    (ok (var-get protocol-fee-rate))
)

;; Private functions
(define-private (calculate-fee (amount uint))
    (/ (* amount (var-get protocol-fee-rate)) u10000)
)

(define-private (update-confidence-score (market-id uint) (new-score uint))
    (match (map-get? markets { market-id: market-id })
        market (map-set markets 
            { market-id: market-id }
            (merge market { confidence-score: new-score })
        )
        false
    )
)

;; Public functions

;; Initialize user balances (for testing/demo purposes)
(define-public (initialize-balance (zen-amount uint) (flux-amount uint))
    (begin
        (map-set zen-balances 
            { user: tx-sender } 
            { balance: zen-amount }
        )
        (map-set flux-balances 
            { user: tx-sender } 
            { balance: flux-amount }
        )
        (ok true)
    )
)

;; Create a new prediction market
(define-public (create-market (question (string-ascii 256)) (end-time uint) (outcome-count uint))
    (let
        (
            (market-id (+ (var-get market-nonce) u1))
        )
        (asserts! (> end-time block-height) err-invalid-outcome)
        (map-set markets
            { market-id: market-id }
            {
                creator: tx-sender,
                question: question,
                end-time: end-time,
                resolved: false,
                winning-outcome: none,
                total-liquidity: u0,
                confidence-score: u50
            }
        )
        (var-set market-nonce market-id)
        (ok market-id)
    )
)

;; Add outcome to a market
(define-public (add-outcome (market-id uint) (outcome-id uint) (description (string-ascii 128)))
    (let
        (
            (market (unwrap! (map-get? markets { market-id: market-id }) err-not-found))
        )
        (asserts! (is-eq (get creator market) tx-sender) err-unauthorized)
        (asserts! (not (get resolved market)) err-market-resolved)
        (map-set market-outcomes
            { market-id: market-id, outcome-id: outcome-id }
            {
                description: description,
                total-staked: u0,
                liquidity-depth: u100
            }
        )
        (ok true)
    )
)

;; Place a prediction
(define-public (predict (market-id uint) (outcome-id uint) (amount uint))
    (let
        (
            (market (unwrap! (map-get? markets { market-id: market-id }) err-not-found))
            (outcome (unwrap! (map-get? market-outcomes { market-id: market-id, outcome-id: outcome-id }) err-invalid-outcome))
            (user-balance (get balance (get-zen-balance tx-sender)))
            (fee (calculate-fee amount))
            (net-amount (- amount fee))
        )
        (asserts! (>= user-balance amount) err-insufficient-balance)
        (asserts! (< block-height (get end-time market)) err-market-closed)
        (asserts! (not (get resolved market)) err-market-resolved)
        
        ;; Deduct ZEN tokens
        (map-set zen-balances
            { user: tx-sender }
            { balance: (- user-balance amount) }
        )
        
        ;; Update user prediction
        (map-set user-predictions
            { user: tx-sender, market-id: market-id, outcome-id: outcome-id }
            {
                amount: (+ (default-to u0 (get amount (map-get? user-predictions { user: tx-sender, market-id: market-id, outcome-id: outcome-id }))) net-amount),
                timestamp: block-height
            }
        )
        
        ;; Update outcome stakes
        (map-set market-outcomes
            { market-id: market-id, outcome-id: outcome-id }
            (merge outcome { total-staked: (+ (get total-staked outcome) net-amount) })
        )
        
        (ok true)
    )
)

;; Provide liquidity to a market
(define-public (provide-liquidity (market-id uint) (amount uint))
    (let
        (
            (market (unwrap! (map-get? markets { market-id: market-id }) err-not-found))
            (user-balance (get balance (get-zen-balance tx-sender)))
            (current-lp (default-to { provided: u0, shares: u0 } (map-get? liquidity-providers { user: tx-sender, market-id: market-id })))
        )
        (asserts! (>= user-balance amount) err-insufficient-balance)
        (asserts! (not (get resolved market)) err-market-resolved)
        
        ;; Deduct ZEN tokens
        (map-set zen-balances
            { user: tx-sender }
            { balance: (- user-balance amount) }
        )
        
        ;; Update LP position
        (map-set liquidity-providers
            { user: tx-sender, market-id: market-id }
            {
                provided: (+ (get provided current-lp) amount),
                shares: (+ (get shares current-lp) amount)
            }
        )
        
        ;; Update market liquidity
        (map-set markets
            { market-id: market-id }
            (merge market { total-liquidity: (+ (get total-liquidity market) amount) })
        )
        
        (ok true)
    )
)

;; Resolve market (Oracle function)
(define-public (resolve-market (market-id uint) (winning-outcome-id uint))
    (let
        (
            (market (unwrap! (map-get? markets { market-id: market-id }) err-not-found))
            (oracle-data (default-to { reputation: u0, total-resolutions: u0 } (map-get? oracles { oracle: tx-sender })))
        )
        (asserts! (>= block-height (get end-time market)) err-market-closed)
        (asserts! (not (get resolved market)) err-market-resolved)
        
        ;; Update market resolution
        (map-set markets
            { market-id: market-id }
            (merge market { 
                resolved: true, 
                winning-outcome: (some winning-outcome-id)
            })
        )
        
        ;; Update oracle reputation
        (map-set oracles
            { oracle: tx-sender }
            {
                reputation: (+ (get reputation oracle-data) u10),
                total-resolutions: (+ (get total-resolutions oracle-data) u1)
            }
        )
        
        (ok true)
    )
)

;; Claim winnings
(define-public (claim-winnings (market-id uint) (outcome-id uint))
    (let
        (
            (market (unwrap! (map-get? markets { market-id: market-id }) err-not-found))
            (prediction (unwrap! (map-get? user-predictions { user: tx-sender, market-id: market-id, outcome-id: outcome-id }) err-not-found))
            (user-balance (get balance (get-zen-balance tx-sender)))
        )
        (asserts! (get resolved market) err-market-closed)
        (asserts! (is-eq (unwrap! (get winning-outcome market) err-invalid-outcome) outcome-id) err-invalid-outcome)
        
        ;; Calculate and distribute winnings (simplified: 2x return)
        (let
            (
                (winnings (* (get amount prediction) u2))
            )
            (map-set zen-balances
                { user: tx-sender }
                { balance: (+ user-balance winnings) }
            )
            
            ;; Clear prediction
            (map-delete user-predictions { user: tx-sender, market-id: market-id, outcome-id: outcome-id })
            
            (ok winnings)
        )
    )
)

;; Stake FLUX for BOND tokens (Temporal Staking)
(define-public (stake-for-bonds (amount uint) (lock-duration uint))
    (let
        (
            (user-flux (get balance (get-flux-balance tx-sender)))
            (current-bonds (get-bond-balance tx-sender))
        )
        (asserts! (>= user-flux amount) err-insufficient-balance)
        
        ;; Deduct FLUX
        (map-set flux-balances
            { user: tx-sender }
            { balance: (- user-flux amount) }
        )
        
        ;; Mint BOND tokens
        (map-set bond-balances
            { user: tx-sender }
            {
                balance: (+ (get balance current-bonds) amount),
                locked-until: (+ block-height lock-duration)
            }
        )
        
        (ok true)
    )
)

;; Register as oracle
(define-public (register-as-oracle)
    (begin
        (map-set oracles
            { oracle: tx-sender }
            {
                reputation: u100,
                total-resolutions: u0
            }
        )
        (ok true)
    )
)

;; Admin function to update protocol fee
(define-public (set-protocol-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-outcome) ;; Max 10% fee
        (var-set protocol-fee-rate new-fee)
        (ok true)
    )
)