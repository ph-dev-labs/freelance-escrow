;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-USER-TYPE (err u103))
(define-constant ERR-ALREADY-VERIFIED (err u104))

;; User types
(define-constant USER-TYPE-FREELANCER u1)
(define-constant USER-TYPE-CLIENT u2)
(define-constant USER-TYPE-ARBITRATOR u3)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Data structures
(define-map users
  principal
  {
    user-type: uint,
    profile-url: (string-ascii 200),
    verified: bool,
    registration-date: uint,
    total-projects: uint,
    total-earnings: uint,
    kyc-verified: bool
  }
)

(define-map user-skills
  principal
  (list 20 (string-ascii 50))
)

(define-map freelancer-stats
  principal
  {
    projects-completed: uint,
    projects-cancelled: uint,
    avg-rating: uint,
    total-disputes: uint
  }
)

(define-map client-stats
  principal
  {
    projects-posted: uint,
    projects-completed: uint,
    total-spent: uint,
    avg-rating: uint
  }
)

(define-data-var total-users uint u0)
(define-data-var total-freelancers uint u0)
(define-data-var total-clients uint u0)

;; Read-only functions
(define-read-only (get-user (user principal))
  (ok (map-get? users user))
)

(define-read-only (get-user-skills (user principal))
  (ok (default-to (list) (map-get? user-skills user)))
)

(define-read-only (get-freelancer-stats (freelancer principal))
  (ok (map-get? freelancer-stats freelancer))
)

(define-read-only (get-client-stats (client principal))
  (ok (map-get? client-stats client))
)

(define-read-only (is-user-verified (user principal))
  (match (map-get? users user)
    user-data (ok (get verified user-data))
    (ok false)
  )
)

(define-read-only (get-user-type (user principal))
  (match (map-get? users user)
    user-data (ok (get user-type user-data))
    (err u404)
  )
)

(define-read-only (get-platform-stats)
  (ok {
    total-users: (var-get total-users),
    total-freelancers: (var-get total-freelancers),
    total-clients: (var-get total-clients)
  })
)

;; Public functions
(define-public (register-user
  (user-type uint)
  (profile-url (string-ascii 200))
  (skills (list 20 (string-ascii 50)))
)
  (let
    (
      (caller tx-sender)
    )
    ;; Check if already registered
    (asserts! (is-none (map-get? users caller)) ERR-ALREADY-REGISTERED)
    
    ;; Validate user type
    (asserts! (or (is-eq user-type USER-TYPE-FREELANCER) 
                  (is-eq user-type USER-TYPE-CLIENT)
                  (is-eq user-type USER-TYPE-ARBITRATOR)) 
              ERR-INVALID-USER-TYPE)
    
    ;; Register user
    (map-set users caller {
      user-type: user-type,
      profile-url: profile-url,
      verified: false,
      registration-date: stacks-block-height,
      total-projects: u0,
      total-earnings: u0,
      kyc-verified: false
    })
    
    ;; Set skills if provided
    (if (> (len skills) u0)
      (map-set user-skills caller skills)
      true
    )
    
    ;; Initialize stats based on user type
    (if (is-eq user-type USER-TYPE-FREELANCER)
      (begin
        (map-set freelancer-stats caller {
          projects-completed: u0,
          projects-cancelled: u0,
          avg-rating: u0,
          total-disputes: u0
        })
        (var-set total-freelancers (+ (var-get total-freelancers) u1))
      )
      (if (is-eq user-type USER-TYPE-CLIENT)
        (begin
          (map-set client-stats caller {
            projects-posted: u0,
            projects-completed: u0,
            total-spent: u0,
            avg-rating: u0
          })
          (var-set total-clients (+ (var-get total-clients) u1))
        )
        true
      )
    )
    
    ;; Increment total users
    (var-set total-users (+ (var-get total-users) u1))
    
    (ok true)
  )
)

(define-public (update-profile
  (profile-url (string-ascii 200))
  (skills (list 20 (string-ascii 50)))
)
  (let
    (
      (caller tx-sender)
      (user-data (unwrap! (map-get? users caller) ERR-NOT-FOUND))
    )
    ;; Update profile
    (map-set users caller (merge user-data {
      profile-url: profile-url
    }))
    
    ;; Update skills
    (if (> (len skills) u0)
      (map-set user-skills caller skills)
      true
    )
    
    (ok true)
  )
)

(define-public (verify-user (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR-NOT-FOUND))
    )
    ;; Only contract owner can verify
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Check if already verified
    (asserts! (not (get verified user-data)) ERR-ALREADY-VERIFIED)
    
    ;; Verify user
    (map-set users user (merge user-data { verified: true }))
    
    (ok true)
  )
)

(define-public (verify-kyc (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR-NOT-FOUND))
    )
    ;; Only contract owner can verify KYC
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Verify KYC
    (map-set users user (merge user-data { kyc-verified: true }))
    
    (ok true)
  )
)

(define-public (increment-project-count (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR-NOT-FOUND))
    )
    (map-set users user (merge user-data {
      total-projects: (+ (get total-projects user-data) u1)
    }))
    (ok true)
  )
)

(define-public (update-earnings (user principal) (amount uint))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR-NOT-FOUND))
    )
    (map-set users user (merge user-data {
      total-earnings: (+ (get total-earnings user-data) amount)
    }))
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

