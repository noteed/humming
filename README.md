# Humming - `queue_classic` in Haskell

## Example usage

    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming create
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming drop
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming enqueue \
        --queue WOUHOU --method play --arguments '{}'
