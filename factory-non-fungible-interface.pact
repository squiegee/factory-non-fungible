(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(interface factory-non-fungible-v1

  (defschema account-details
    @doc
      " Account details: token ID, account name, balance, and guard."
    @model
      [ (invariant (!= id ""))
        (invariant (!= account ""))
        (invariant (>= balance 0.0))
      ]
    id:string
    account:string
    balance:decimal
    guard:guard)

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc
      " Manage transferring AMOUNT of ID from SENDER to RECEIVER. \
      \ As event, also used to notify burn (with \"\" RECEIVER) \
      \ and create (with \"\" SENDER)."
    @managed amount TRANSFER-mgr
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    @doc " Manages TRANSFER cap AMOUNT where MANAGED is the installed quantity \
         \ and REQUESTED is the quantity attempting to be granted."
  )

  (defcap URI:bool (id:string uri:string)
    @doc " Emitted when URI is updated, if supported."
    @event
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted when supply is updated, if supported."
    @event
  )

  (defun precision:integer (id:string)
    @doc
      " Return maximum decimal precision for ID."
  )

  (defun enforce-unit:bool
    ( id:string
      amount:decimal
    )
    @doc
      " Enforce that AMOUNT meets minimum precision allowed for ID."
  )

  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    @doc
      " Create ACCOUNT for ID with 0.0 balance, with GUARD controlling access."
    @model
      [ (property (!= id ""))
        (property (!= account ""))
      ]
  )

  (defun get-balance:decimal
    ( id:string
      account:string
    )
    @doc
      " Get balance of ID for ACCOUNT. Fails if account does not exist."
  )

  (defun details:object{account-details}
    ( id:string
      account:string
    )
    @doc
      " Get details of ACCOUNT under ID. Fails if account does not exist."
  )

  (defun rotate:string
    ( id:string
      account:string
      new-guard:guard )
    @doc
      " Rotate guard for ACCOUNT for ID to NEW-GUARD, validating against existing guard."
    @model
      [ (property (!= id ""))
        (property (!= account ""))
      ]

  )

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
      \ Fails if SENDER does not exist. Managed by TRANSFER."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= sender receiver))
      ]
  )

  (defun transfer-create:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER and RECEIVER. \
      \ If RECEIVER exists, RECEIVER-GUARD must match existing guard; \
      \ if RECEIVER does not exist, account is created. \
      \ Managed by TRANSFER."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= sender receiver))
      ]
  )

  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal
    )
    @doc
      " Transfer AMOUNT of ID between accounts SENDER on source chain \
      \ and RECEIVER on TARGET-CHAIN. If RECEIVER exists, RECEIVER-GUARD \
      \ must match existing guard. If RECEIVER does not exist, account is created."
    @model
      [ (property (> amount 0.0))
        (property (!= id ""))
        (property (!= sender ""))
        (property (!= receiver ""))
        (property (!= target-chain ""))
      ]
  )

  (defun total-supply:decimal (id:string)
    @doc
      " Give total available quantity of ID. If not supported, return 0."
  )

  (defun uri:string (id:string)
    @doc
      " Give URI for ID. If not supported, return \"\" (empty string)."
  )

  (defun collection-total-supply:decimal (collection-id:string)
    @doc
      " Gets a collections total supply "
  )

  (defun buy:string ( id:string account:string guard:guard amount:decimal )
    @doc
      " Buys a NFT off the marketplace "
  )

  (defun sell:string (id:string account:string price:decimal forsale:bool)
    @doc
      " Adds/Removes a NFT marketoffer to/from the NFT marketplace "
  )

  (defun get-user-details:string (user-id:string)
    @doc
      " Gets detailed list of a users NFTs"
  )

  (defun get-nft-details:string (nft-id:string)
    @doc
      " Gets a NFTs details "
  )

  (defun get-user-nfts:string (user-id:string)
    @doc
      " Gets list of user NFT ids"
  )

  (defun check-if-user-has-nft:bool (userid:string nft-id:string)
    @doc
      " Returns true/false if user has NFT "
  )

  (defun check-if-user-has-collection:bool (userid:string collection-id:string)
    @doc
      " Returns true/false if user owns an piece of a collection "
  )

  (defun check-if-nft-id-in-collection:bool (collection-id:string nft-id:string)
    @doc
      " Returns true/false if a collection contains an NFT "
  )

  (defun get-collection-details:string(collection-id:string)
    @doc
      " Gets collection details "
  )

  (defun get-collection-nft-ids:[string] (collection-id:string)
    @doc
      " Gets list of all NFT IDs in a collection "
  )

  (defun get-market-details:string ()
    @doc
      " Gets detailed list of market offers "
  )

  (defun get-marketoffer-details:string (nft-id:string)
    @doc
      " Gets a market offers details by NFT ID"
  )

  (defun get-nft-ids-forsale:string ()
    @doc
      " Gets list of NFT ids for sale "
  )

)
