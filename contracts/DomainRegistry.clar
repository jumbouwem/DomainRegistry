;; DomainRegistry: Digital Domain Name Ownership and Transfer System
;; Version: 1.0.0

(define-constant ERR-NOT-REGISTRANT (err u1))
(define-constant ERR-DOMAIN-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-REGISTERED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-DURATION (err u5))
(define-constant ERR-INVALID-EXTENSION (err u6))
(define-constant ERR-INVALID-PURPOSE (err u7))
(define-constant ERR-INVALID-NAME (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))

(define-constant MIN-DURATION u1)

(define-data-var next-domain-id uint u1)

(define-map domains
    uint
    {
        registrant: principal,
        domain-name: (string-utf8 63),
        domain-description: (string-utf8 200),
        domain-extension: (string-utf8 8),
        domain-purpose: (string-utf8 20),
        registration-status: (string-utf8 15),
        registration-duration: uint
    }
)

(define-private (validate-extension (extension (string-utf8 8)))
    (or 
        (is-eq extension u".com")
        (is-eq extension u".org")
        (is-eq extension u".net")
        (is-eq extension u".io")
        (is-eq extension u".app")
        (is-eq extension u".dev")
    )
)

(define-private (validate-purpose (purpose (string-utf8 20)))
    (or 
        (is-eq purpose u"Business")
        (is-eq purpose u"Personal")
        (is-eq purpose u"Portfolio")
        (is-eq purpose u"E-commerce")
        (is-eq purpose u"Blog")
    )
)

(define-private (validate-text-length (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-public (register-domain-name 
    (domain-name (string-utf8 63))
    (domain-description (string-utf8 200))
    (domain-extension (string-utf8 8))
    (domain-purpose (string-utf8 20))
    (registration-duration uint))
    (let
        (
            (domain-id (var-get next-domain-id))
        )
        (asserts! (validate-text-length domain-name u2 u63) ERR-INVALID-NAME)
        (asserts! (validate-text-length domain-description u10 u200) ERR-INVALID-DESCRIPTION)
        (asserts! (>= registration-duration MIN-DURATION) ERR-INVALID-DURATION)
        (asserts! (validate-extension domain-extension) ERR-INVALID-EXTENSION)
        (asserts! (validate-purpose domain-purpose) ERR-INVALID-PURPOSE)
        
        (map-set domains domain-id {
            registrant: tx-sender,
            domain-name: domain-name,
            domain-description: domain-description,
            domain-extension: domain-extension,
            domain-purpose: domain-purpose,
            registration-status: u"active",
            registration-duration: registration-duration
        })
        (var-set next-domain-id (+ domain-id u1))
        (ok domain-id)
    )
)

(define-public (transfer-domain-ownership (domain-id uint) (new-registrant principal))
    (let
        (
            (domain (unwrap! (map-get? domains domain-id) ERR-DOMAIN-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get registrant domain)) ERR-NOT-REGISTRANT)
        (asserts! (is-eq (get registration-status domain) u"active") ERR-INVALID-STATUS)
        (ok (map-set domains domain-id (merge domain { registrant: new-registrant, registration-status: u"transferred" })))
    )
)

(define-read-only (get-domain (domain-id uint))
    (ok (map-get? domains domain-id))
)

(define-read-only (get-domain-registrant (domain-id uint))
    (ok (get registrant (unwrap! (map-get? domains domain-id) ERR-DOMAIN-NOT-FOUND)))
)