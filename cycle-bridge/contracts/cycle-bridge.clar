;; CycleBridge - Decentralized Performance Rights Ecosystem
;; Performance Cycle Token (PCT) Management System

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-insufficient-balance (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (basis points)
(define-data-var next-work-id uint u1)
(define-data-var next-performance-id uint u1)

;; Creative Work Structure
(define-map creative-works
  { work-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    work-hash: (buff 32),
    total-performances: uint,
    total-revenue: uint,
    reputation-score: uint,
    created-at: uint,
    active: bool
  }
)

;; Performance Cycle Token (PCT) Structure
(define-map performance-tokens
  { performance-id: uint }
  {
    work-id: uint,
    performer: principal,
    performance-type: (string-ascii 20), ;; "live", "streaming", "sync", "derivative"
    usage-count: uint,
    revenue-generated: uint,
    timestamp: uint,
    verified: bool
  }
)

;; Royalty Split Configuration
(define-map royalty-splits
  { work-id: uint, stakeholder: principal }
  {
    percentage: uint, ;; basis points (1-10000)
    role: (string-ascii 20) ;; "creator", "performer", "producer", "publisher"
  }
)

;; Creator Reputation Scores
(define-map creator-reputation
  { creator: principal }
  {
    total-works: uint,
    total-revenue: uint,
    average-performance: uint,
    reputation-score: uint
  }
)

;; Revenue Balance Tracking
(define-map revenue-balances
  { work-id: uint, stakeholder: principal }
  { balance: uint }
)

;; Licensing Agreements
(define-map licensing-agreements
  { work-id: uint, licensee: principal }
  {
    license-type: (string-ascii 20),
    fee: uint,
    start-block: uint,
    end-block: uint,
    active: bool
  }
)

;; Read-only functions

(define-read-only (get-creative-work (work-id uint))
  (map-get? creative-works { work-id: work-id })
)

(define-read-only (get-performance-token (performance-id uint))
  (map-get? performance-tokens { performance-id: performance-id })
)

(define-read-only (get-royalty-split (work-id uint) (stakeholder principal))
  (map-get? royalty-splits { work-id: work-id, stakeholder: stakeholder })
)

(define-read-only (get-creator-reputation (creator principal))
  (default-to 
    { total-works: u0, total-revenue: u0, average-performance: u0, reputation-score: u0 }
    (map-get? creator-reputation { creator: creator })
  )
)

(define-read-only (get-revenue-balance (work-id uint) (stakeholder principal))
  (default-to 
    { balance: u0 }
    (map-get? revenue-balances { work-id: work-id, stakeholder: stakeholder })
  )
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Public functions

;; Register a new creative work
(define-public (register-creative-work (title (string-ascii 100)) (work-hash (buff 32)))
  (let
    (
      (work-id (var-get next-work-id))
    )
    (asserts! (is-eq tx-sender tx-sender) err-unauthorized)
    
    ;; Create the work
    (map-set creative-works
      { work-id: work-id }
      {
        creator: tx-sender,
        title: title,
        work-hash: work-hash,
        total-performances: u0,
        total-revenue: u0,
        reputation-score: u0,
        created-at: block-height,
        active: true
      }
    )
    
    ;; Set default royalty split (100% to creator)
    (map-set royalty-splits
      { work-id: work-id, stakeholder: tx-sender }
      { percentage: u10000, role: "creator" }
    )
    
    ;; Update creator reputation
    (update-creator-stats tx-sender u1 u0)
    
    ;; Increment work ID
    (var-set next-work-id (+ work-id u1))
    
    (ok work-id)
  )
)

;; Add royalty split stakeholder
(define-public (add-royalty-stakeholder 
  (work-id uint) 
  (stakeholder principal) 
  (percentage uint) 
  (role (string-ascii 20)))
  (let
    (
      (work (unwrap! (get-creative-work work-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator work)) err-unauthorized)
    (asserts! (<= percentage u10000) err-invalid-amount)
    
    (map-set royalty-splits
      { work-id: work-id, stakeholder: stakeholder }
      { percentage: percentage, role: role }
    )
    
    (ok true)
  )
)

;; Record a performance and generate PCT
(define-public (record-performance 
  (work-id uint) 
  (performance-type (string-ascii 20))
  (usage-count uint))
  (let
    (
      (work (unwrap! (get-creative-work work-id) err-not-found))
      (performance-id (var-get next-performance-id))
    )
    (asserts! (get active work) err-unauthorized)
    
    ;; Create performance token
    (map-set performance-tokens
      { performance-id: performance-id }
      {
        work-id: work-id,
        performer: tx-sender,
        performance-type: performance-type,
        usage-count: usage-count,
        revenue-generated: u0,
        timestamp: block-height,
        verified: false
      }
    )
    
    ;; Update work statistics
    (map-set creative-works
      { work-id: work-id }
      (merge work { total-performances: (+ (get total-performances work) u1) })
    )
    
    ;; Increment performance ID
    (var-set next-performance-id (+ performance-id u1))
    
    (ok performance-id)
  )
)

;; Distribute royalties for a performance
(define-public (distribute-royalties (work-id uint) (performance-id uint) (total-amount uint))
  (let
    (
      (work (unwrap! (get-creative-work work-id) err-not-found))
      (performance (unwrap! (get-performance-token performance-id) err-not-found))
      (platform-fee (/ (* total-amount (var-get platform-fee-percentage)) u10000))
      (distributable-amount (- total-amount platform-fee))
    )
    (asserts! (is-eq work-id (get work-id performance)) err-unauthorized)
    (asserts! (> total-amount u0) err-invalid-amount)
    
    ;; Update performance revenue
    (map-set performance-tokens
      { performance-id: performance-id }
      (merge performance { 
        revenue-generated: (+ (get revenue-generated performance) total-amount),
        verified: true 
      })
    )
    
    ;; Update work total revenue
    (map-set creative-works
      { work-id: work-id }
      (merge work { total-revenue: (+ (get total-revenue work) total-amount) })
    )
    
    ;; Update creator reputation
    (update-creator-stats (get creator work) u0 total-amount)
    
    (ok true)
  )
)

;; Claim accumulated royalties
(define-public (claim-royalties (work-id uint))
  (let
    (
      (balance-info (get-revenue-balance work-id tx-sender))
      (amount (get balance balance-info))
    )
    (asserts! (> amount u0) err-insufficient-balance)
    
    ;; Reset balance
    (map-set revenue-balances
      { work-id: work-id, stakeholder: tx-sender }
      { balance: u0 }
    )
    
    ;; Transfer would happen here in a real implementation with STX transfers
    (ok amount)
  )
)

;; Create licensing agreement
(define-public (create-license 
  (work-id uint) 
  (licensee principal) 
  (license-type (string-ascii 20))
  (fee uint)
  (duration uint))
  (let
    (
      (work (unwrap! (get-creative-work work-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator work)) err-unauthorized)
    (asserts! (> fee u0) err-invalid-amount)
    
    (map-set licensing-agreements
      { work-id: work-id, licensee: licensee }
      {
        license-type: license-type,
        fee: fee,
        start-block: block-height,
        end-block: (+ block-height duration),
        active: true
      }
    )
    
    (ok true)
  )
)

;; Update platform fee (owner only)
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

;; Private functions

(define-private (update-creator-stats (creator principal) (works-delta uint) (revenue-delta uint))
  (let
    (
      (current-stats (get-creator-reputation creator))
      (new-total-works (+ (get total-works current-stats) works-delta))
      (new-total-revenue (+ (get total-revenue current-stats) revenue-delta))
      (new-avg (if (> new-total-works u0) 
                   (/ new-total-revenue new-total-works) 
                   u0))
      (new-score (calculate-reputation-score new-total-works new-total-revenue))
    )
    (map-set creator-reputation
      { creator: creator }
      {
        total-works: new-total-works,
        total-revenue: new-total-revenue,
        average-performance: new-avg,
        reputation-score: new-score
      }
    )
  )
)

(define-private (calculate-reputation-score (total-works uint) (total-revenue uint))
  ;; Simple reputation calculation: works * 10 + revenue / 1000
  (+ (* total-works u10) (/ total-revenue u1000))
)