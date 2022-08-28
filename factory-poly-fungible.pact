(namespace (read-msg 'ns))
(module factory-poly-fungible GOVERNANCE

;\|/          (__)       factory-non-fungible | poly-fungible-v1 mod
;     `\------(oo)       + mints non fungible token collections
;       ||    (__)       + mints non fungible tokens
;       ||w--||     \|/  + includes marketplace
;=====================================================================================
;Step 1) Create your NFT collection with create-collection()
;Step 2) Mint your NFTs into your collection with mint() or mass-mint()
;====================================================================================

  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-kadena-stake) )

  ;/////////////////
  ;POLY-FUNGIBLE-V1
  ;\\\\\\\\\\\\\\\\\

  (use fungible-util)
  (implements factory-non-fungible-v1)

  ;PF1 SCHEMAS + TABLES

  (defschema entry
    id:string
    account:string
    balance:decimal
    guard:guard
    )

  (deftable ledger:{entry})

  (defschema supply
    supply:decimal
    )

  (deftable supplies:{supply})

  ;POLYFUNGIV1 CONSTANTS + CAPS

  (defconst MINIMUM_PRECISION 0)

  (defcap DEBIT (id:string sender:string)
    (enforce-guard
      (at 'guard
        (read ledger (key id sender)))))

  (defcap CREDIT (id:string receiver:string) true)

  (defcap ADD_ID_COLLECTION (id:string collection-id:string) true)

  (defcap REMOVE_ID_COLLECTION (id:string collection-id:string) true)

  (defcap CREATOR_GUARD (collection-id:string)
    (enforce-guard (at 'guard (coin.details (at 'creator (read collections-table collection-id)))) )
  )

  (defcap URI:bool (id:string uri:string) @event true)

  (defcap SUPPLY:bool (id:string supply:decimal) @event true)

  (defun total-supply:decimal (id:string)
    (with-default-read supplies id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (enforce
      (= (floor amount (precision id))
         amount)
      "precision violation")
  )

  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
    )

  (defun get-balance:decimal (id:string account:string)
    (at 'balance (read ledger (key id account)))
    )

  (defun details:object{factory-non-fungible-v1.account-details}
    ( id:string account:string )
    (read ledger (key id account))
    )

  (defun rotate:string (id:string account:string new-guard:guard)
    (with-read ledger (key id account)
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update ledger (key id account)
        { "guard" : new-guard }))
    )


  (defun precision:integer (id:string)
    MINIMUM_PRECISION)

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)


    (with-capability (TRANSFER id sender receiver amount)
      (debit id sender amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (credit id receiver g amount))
      )
    )

  (defun transfer-create:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)

    (with-capability (TRANSFER id sender receiver amount)
      (debit id sender amount)
      (credit id receiver receiver-guard amount))
    )

  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (enforce false "cross chain not supported"))
   )

  (defun uri:string (id:string)
    (let
        (
            (uri-data (read nft-uris id) )
        )
        uri-data
    )
  )

  ;/////////////////
  ;STAKE FACTORY
  ;\\\\\\\\\\\\\\\\\

  ;SCHEMAS + TABLES 

  ;General collection definition schema
  (defschema collection
    @doc "key collection-id"
    collection-id:string
    creator:string
    supply:decimal
    hash:string
    link:string
  )

  ;Unique signatures list
  (defschema signatures-schema
    @doc "key stakefactory"
    collection-id:string
  )

  ;list of nft ids in collection
  (defschema collection-id
    @doc "key collection-id"
    ids:[string]
  )

  ;for nft-id -> collection records
  (defschema nft-collection
    @doc "key nft-id"
    collection-id:string
  )

  ;list of nfts user owns
  (defschema user-nfts
    @doc "key user-id"
    ids:[string]
  )

  ;list of collections user is in
  (defschema user-collections
    @doc "key user account"
    collection-ids:[string]
  )

  ;all collections
  (defschema all-collections-schema
    @doc "key stakefactory"
    ids:[string]
  )

  ;marketplace schema
  (defschema marketoffer
    @doc "key nft-id"
    account:string
    forsale:bool
    price:decimal
    token:module{fungible-v2}
  )

  ;nft uri schema
  (defschema uri-schema
    @doc "key nft-id"
    id:string
    collection-id:string
    name:string
    description:string
    image:string
    attributes:object{attribute}
  )

  ;nft attribute schema in uris
  (defschema attribute
    @doc "key nft-id"
    a1:[string]
    a2:[decimal]
    a3:[integer]
  )

  ;Collectable Tables
  (deftable collections-table:{collection})
  (deftable collection-ids-table:{collection-id})
  (deftable nft-collections:{nft-collection})
  (deftable user-nfts-table:{user-nfts})
  (deftable user-collections-table:{user-collections})
  (deftable nft-uris:{uri-schema})
  (deftable nft-attributes:{attribute})
  (deftable all-collections:{all-collections-schema})
  (deftable signatures:{signatures-schema})

  ;Marketplace Tables
  (deftable nft-marketoffers:{marketoffer})

  (defschema nfts-forsale-schema
    @doc "key stakefactory"
    ids:[string]
  )

  (deftable nfts-forsale-table:{nfts-forsale-schema})

  ;CAPABILITIES

  (defcap ACCOUNT_GUARD ( id:string account:string )
    @doc " Look up the guard for an account. "
    (enforce-guard
      (at 'guard
      (read ledger (key id account))))
  )

  (defcap STAKEFACTORY_MARKET_SELL (id:string account:string price:decimal forsale:bool)
    @doc " Emitted event when a NFT is sold "
    @event true
  )

  (defcap STAKEFACTORY_MARKET_BUY (id:string buyer:string seller:string price:decimal)
    @doc " Emitted event when a NFT is purchased "
    @event true
  )

  (defcap UPDATE ()
  true)

  ;////////////
  ;MARKETPLACE
  ;\\\\\\\\\\\\

  (defun sell:string ( id:string account:string price:decimal forsale:bool)
    @doc " Add or remove a token from marketplace "
    (with-read ledger (key id account)
      { 'id := l_id
      , 'account := l_account
      , 'balance := l_balance
      , 'guard := l_guard }
      ;Enforce rules
      (with-capability (ACCOUNT_GUARD id l_account)
        (enforce-guard l_guard)
        (enforce (= account l_account) "Account Owners dont match.")
        (enforce (> l_balance 0.0) "Insufficient funds.")
        (enforce (> price 0.0)  "Positive decimal sell prices only." )
        (write nft-marketoffers id
          { 'account: account
          , 'price: price
          , 'forsale: forsale
          , 'token: test.stake-token
          })
        ;Add nft id to active sales list
        (with-default-read nfts-forsale-table "stakefactory"
          { "ids" : [] }
          { "ids" := t_ids }
            (write nfts-forsale-table "stakefactory" {"ids": (+ [id] t_ids ) } )
        )
        (emit-event (STAKEFACTORY_MARKET_SELL id account price forsale))
        (if (= forsale true) (format "NFT ID {} is now for sale for {}" [id price]) (format "NFT ID {} is no longer for sale" [id]))
      )
    )
  )

  (defun buy:string
    ( id:string
      account:string
      guard:guard
      amount:decimal )
    @doc " Buy a NFT off the Market "
    (with-read nft-marketoffers id
          { 'account := m_account
          , 'price := m_price
          , 'forsale := m_forsale
          , 'token := m_token:module{fungible-v2} }
        (enforce (= m_forsale true)  "You can only purchase a NFT that is for sale." )
        (enforce (= amount m_price) "Insufficient funds.")
        (enforce (!= account m_account) "You cannot buy your own NFT.")
        (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts.")
        (enforce-coin-account-exists account)
    	  (let ((cur_guard (coin-account-guard account)))
        (enforce (= cur_guard guard) "Account guards must match the same account guard in the coin contract."))
        (m_token::transfer account m_account amount)
        (update ledger (key id m_account)
          { "balance" : 0.0 })
          (write ledger (key id account)
            { "balance" : 1.0
            , "guard"   : guard
            , "id"   : id
            , "account" : account
            }
          )
          ;Update market
          (write nft-marketoffers id
              { 'account: account
              , 'price: 98765.43
              , 'forsale: false
              })
          ;Update active sales list
          (with-default-read nfts-forsale-table "stakefactory"
            { "ids" : [] }
            { "ids" := t_ids }
            (let*
                (
                  (newlist:[string]  (filter (compose (composelist) (!= id)) t_ids) )
                )
                (write nfts-forsale-table "stakefactory" {"ids": newlist } )
            )
          )
          ;Remove collection from old user
          (with-read nft-collections id
            { "collection-id" := t_collection-id }
            (if (= (contains true (map (check-if-nft-id-in-collection t_collection-id) (get-user-nfts m_account)) ) true)
            (with-default-read user-collections-table m_account
              { "collection-ids" : [] }
              { "collection-ids" := t_cids }
              (let*
                  (
                    (newlist:[string]  (filter (compose (composelist) (!= t_collection-id)) t_cids) )
                  )
                  (write user-collections-table m_account {"collection-ids": newlist } )
              )
            )
            true
            )
          )
          ;Remove nft id from old user
          (with-default-read user-nfts-table m_account
            { "ids" : [] }
            { "ids" := t_userids }
            (let*
                (
                  (newlist:[string]  (filter (compose (composelist) (!= id)) t_userids) )
                )
                (write user-nfts-table m_account {"ids": newlist } )

            )
          )
          ;Add nft to new user
          (with-read nft-collections id
            { "collection-id" := t_collection-id }

            ;Record nft id to user
            (with-default-read user-nfts-table account
              { "ids" : [] }
              { "ids" := t_ids }
              (write user-nfts-table account {"ids": (+ [id] t_ids ) } )
            )

            ;Record collection id to user
            (with-default-read user-collections-table account
              { "collection-ids" : [] }
              { "collection-ids" := t_collection-ids }
              (if (= (check-if-user-has-collection account t_collection-id) false)
                (write user-collections-table account {"collection-ids": (+ [t_collection-id] t_collection-ids ) } )
              true)
            )

          )
        (emit-event (STAKEFACTORY_MARKET_BUY id account m_account amount))
        (format " Purchased a NFT ID {} for {} KDA " [id amount])
    )
  )



  ;////////////
  ;NFT CREATION
  ;\\\\\\\\\\\\

  (defun create-collection
    ( collection-id:string
      creator:string
      link:string
      signature:string
    )
    @doc " Create a unique NFT collection "

    (insert collections-table collection-id
      { "collection-id" : collection-id
      , "creator"   : creator
      , "supply" : 0.0
      , "hash" : (hash-sig signature)
      , "link" : link
      })

    (insert signatures signature {"collection-id" : collection-id})

    (with-default-read all-collections "stakefactory"
      { "ids" : [] }
      { "ids" := t_ids }
      (write all-collections "stakefactory" {"ids": (+ [collection-id] t_ids ) } )
    )
  )

  ;nft uri schema
  (defschema mintable
    @doc "mintable"
    id:string
    collection-id:string
    name:string
    description:string
    image:string
    a1:[string]
    a2:[decimal]
    a3:[integer]
  )

  (defun mass-minter (account:string account-guard:guard mintable:object{mintable})
    @doc " Mint multiple NFTs utility function "
    (bind mintable {
                        "id" := m_id,
                        "collection-id" := m_collection-id,
                        "name" := m_name,
                        "description" := m_description,
                        "image" := m_image,
                        "a1" := m_a1,
                        "a2" := m_a2,
                        "a3" := m_a3
                      }
                      (mint m_collection-id m_id account account-guard m_name m_description m_image m_a1 m_a2 m_a3 )
    )
  )


  (defun mass-mint
    (
      account:string
      account-guard:guard
      mintables:[object:{mintable}]
    )
    @doc " Mint multiple nfts into a collection "
    (map (mass-minter account account-guard) mintables )
  )


  (defun mint:string
    ( collection-id:string
      id:string
      account:string
      guard:guard
      name:string
      description:string
      image:string
      a1:[string]
      a2:[decimal]
      a3:[integer]
    )
    @doc " Mint an NFT into a collection "
    (with-capability (CREATOR_GUARD collection-id)
    ;Make nft attribute
        (let
            (
                (attribute {
                   'a1: a1
                  , 'a2: a2
                  , 'a3: a3
                  })
            )
            ;Insert nft attribute
            (insert nft-attributes id attribute)

            ;Insert nft uri
            (insert nft-uris id
            { "id" : id
            , "collection-id" : collection-id
            , "name" : name
            , "description" : description
            , "image" : image
            , "attributes" : attribute
            })

            ;Tag collection + id
            (insert nft-collections id { "collection-id" : collection-id} )

        )

        ;Add nft id to collection
        (with-default-read collection-ids-table collection-id
          { "ids" : [] }
          { "ids" := t_ids }
            (write collection-ids-table collection-id {"ids": (+ [id] t_ids ) } )
        )

        ;Increase collection supply
        (with-default-read collections-table collection-id
          { "supply" : 0.0 }
          { "supply" := t_supply }
          (update collections-table collection-id {"supply": (+ t_supply 1.0) } )
        )

        ;Credit new token
        (with-capability (CREDIT id account)
          (credit id account guard 1.0))

        (format "Minted token {}" [id])

      )
  )

  (defun credit:string
    (
      id:string
      account:string
      guard:guard
      amount:decimal
    )

    (require-capability (CREDIT id account))

    (enforce-unit id amount)

    (with-default-read ledger (key id account)
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (write ledger (key id account)
        { "balance" : (+ balance amount)
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })

      (with-capability (UPDATE)
      (update-supply id amount)
      )

      )

      (with-read nft-collections id
        { "collection-id" := t_collection-id }

        ;Record nft id to user
        (with-default-read user-nfts-table account
          { "ids" : [] }
          { "ids" := t_ids }
          (write user-nfts-table account {"ids": (+ [id] t_ids ) } )
        )

        ;Record collection id to user
        (with-default-read user-collections-table account
          { "collection-ids" : [] }
          { "collection-ids" := t_collection-ids }
          (if (= (check-if-user-has-collection account t_collection-id) false)
            (write user-collections-table account {"collection-ids": (+ [t_collection-id] t_collection-ids ) } )
          true)
        )

      )
  )

  (defun debit:string
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    ;Remove collection from user if needed
    (with-read nft-collections id
      { "collection-id" := t_collection-id }
      (if (= (contains true (map (check-if-nft-id-in-collection t_collection-id) (get-user-nfts account)) ) true)
      (with-default-read user-collections-table account
        { "collection-ids" : [] }
        { "collection-ids" := t_ids }
        (let*
            (
              (newlist:[string]  (filter (compose (composelist) (!= t_collection-id)) t_ids) )
            )
            (write user-collections-table account {"collection-ids": newlist } )
        )
      )
      true
      )
    )

    ;Remove nft id from user
    (with-default-read user-nfts-table account
      { "ids" : [] }
      { "ids" := t_ids }
      (let*
          (
            (newlist:[string]  (filter (compose (composelist) (!= id)) t_ids) )
          )
          (write user-nfts-table account {"ids": newlist } )

      )
    )

    (with-read ledger (key id account)
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger (key id account)
        { "balance" : (- balance amount) }
        ))

    (with-capability (UPDATE)
     (update-supply id (- amount))
    )
  )

  (defun update-supply (id:string amount:decimal)
  (require-capability (UPDATE))
    (with-default-read supplies id
      { 'supply: 0.0 }
      { 'supply := s }
      (write supplies id {'supply: (+ s amount)}))
  )

  ;GETTERS

  (defun get-all-nft-ids ()
    "Get all nft ids"
    (keys supplies))

  (defun get-user-nfts:string (user-id:string)
      @doc " Gets list of NFT ids owned by a user "
          (with-default-read user-nfts-table user-id
                  { "ids" : [] }
                  { "ids" := t_ids }
                  (let
                          (
                              (user-nftids (map (composelist) t_ids) )
                          )
                          user-nftids
                      )
          )
  )

  (defun get-user-details:string (user-id:string)
      @doc " Gets list of NFT details owned by a user "
          (with-default-read user-collections-table user-id
                  { "collection-ids" : [] }
                  { "collection-ids" := t_ids }
                  (let
                      (
                          (user-nft-ids (get-user-nfts user-id) )
                      )
                      (map (get-nft-details) user-nft-ids)
                  )
          )
  )

  (defun get-nft-details:string (nft-id:string)
      @doc " Gets a NFT details "
          (let
              (
                  (uri-data (read nft-uris nft-id) )
              )
              uri-data
          )
  )

  (defun get-collection-details:string(collection-id:string)
      @doc " Gets a Collections details "
          (let
              (
                  (collection-data (read collections-table collection-id) )
              )
              collection-data
          )
  )

  (defun get-collection-nft-ids:[string] (collection-id:string)
    @doc " Gets all nft ids in a collection "
   (at "ids" (read collection-ids-table collection-id))
  )

  (defun get-signature-collection:string (signature:string)
    @doc " Gets a collection from a signature "
   (at "collection-id" (read signatures signature))
  )

  (defun collection-total-supply:decimal (collection-id:string)
    @doc " Gets a collections total supply "
    (with-default-read collections-table collection-id
      { 'max-supply : 0.0 }
      { 'max-supply := s }
      s)
  )

  (defun get-nft-ids-forsale:string ()
      @doc " Gets list of NFT ids for sale "
          (with-default-read nfts-forsale-table "stakefactory"
                  { "ids" : [] }
                  { "ids" := t_ids }
                  (let
                          (
                              (idsforsale (map (composelist) t_ids) )
                          )
                          idsforsale
                      )
          )
  )

  (defun get-market-details:string ()
      @doc " Gets detailed list of NFTs for sale on market "
      (let
          (
              (user-nft-ids (get-nft-ids-forsale) )
          )
          (map (get-marketoffer-details) user-nft-ids)
      )
  )

  (defun get-marketoffer-details:string (nft-id:string)
      @doc " Gets marketoffer data by nft-id "
          (let
              (
                  (uri-data (read nft-uris nft-id) )
                  (market-data (read nft-marketoffers nft-id) )
              )
              (+ uri-data market-data)
          )
  )



  ;UTILITIES

  (defun key ( id:string account:string )
    (format "{}:{}" [id account])
  )

  (defun enforce-coin-account-exists (account:string)
  @doc "Enforces coin account existance"
   (let ((exist (coin-account-exists account)))
      (enforce exist "Account does not exist in coin contract")))

  (defun coin-account-guard (account:string)
    @doc "Enforces coin account guard"
    (at "guard" (coin.details account)))

  (defun coin-account-exists:bool (account:string)
    @doc "Returns true if account exists on coin contract"
	(try false
	     (let ((ok true))
		      (coin.details account)
			  ok)))

  (defun composelist
    (
      stringlist:string
    )
    @doc " Utility to compose lists "
    (let*
      (
        (current:string stringlist)
      )
      current
    )
  )

  (defun check-if-nft-id-in-collection:bool (collection-id:string nft-id:string)
    @doc " Returns true/false if a NFT ID is apart of a collection ID "
      (with-default-read collection-ids-table collection-id
        {"ids": []}
        {"ids":= t_ids}
        (contains nft-id t_ids)
        )
  )

  (defun check-if-user-has-collection:bool (userid:string collection-id:string)
    @doc " Returns true/false if a user has a collection ID "
      (with-default-read user-collections-table userid
        {"collection-ids": []}
        {"collection-ids":= t_ids}
        (contains collection-id t_ids)
        )
  )

  (defun check-if-user-has-nft:bool (userid:string nft-id:string)
    @doc " Returns true/false if a user has a NFT ID "
      (with-default-read user-nfts-table userid
        {"ids": []}
        {"ids":= t_ids}
        (contains nft-id t_ids)
        )
  )

  (defun hash-sig:string
    ( sig:string )
    @doc " Hashes a creators signature to a collection "
    (let ((x (base64-encode sig)))
          (hash x)
    )
  )

)

;(create-table factory-poly-fungible-reference.ledger)
;(create-table factory-poly-fungible-reference.supplies)
;(create-table ledger)
;(create-table supplies)
;(create-table collections-table)
;(create-table collection-ids-table)
;(create-table nft-collections)
;(create-table nfts-forsale-table)

;(create-table user-collections-table)
;(create-table user-nfts-table)
;(create-table nft-marketoffers)
;(create-table nft-uris)
;(create-table nft-attributes)
;(create-table all-collections)
;(create-table signatures)
