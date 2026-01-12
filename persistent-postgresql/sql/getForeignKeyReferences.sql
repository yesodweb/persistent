-- Get all foreign key references among the given set of table names in the
-- current namespace/schema. This query is used by the migrator to check whether
-- foreign key definitions are up to date.
--
-- This query takes one parameter: an array of table names.
with
    foreign_constraints as (
        select
            c.*
        from
            pg_constraint AS c
        inner join pg_class src_table
            on src_table.oid = c.conrelid
        inner join pg_namespace ns
            on ns.oid = c.connamespace
        where
            -- f = foreign key constraint
            c.contype = 'f'
            and src_table.relname = ANY (?)
            and ns.nspname = current_schema()
    ),
    foreign_constraint_with_source_columns as (
        select
            c.oid,
            array_agg(
                a.attname::text
                ORDER BY
                    k.n ASC
            ) as column_names
        from
            foreign_constraints AS c
            -- conkey is a list of the column indices on the source
            -- table
            CROSS JOIN LATERAL unnest(c.conkey) WITH ORDINALITY AS k (attnum, n)
            INNER JOIN pg_attribute AS a
                -- conrelid is the id of the source table
                ON k.attnum = a.attnum AND c.conrelid = a.attrelid
        group by
            c.oid
    ),
    foreign_constraint_with_foreign_columns as (
        select
            c.oid,
            array_agg(
                a.attname::text
                ORDER BY
                    k.n ASC
            ) as foreign_column_names
        from
            foreign_constraints AS c
            -- confkey is a list of the column indices on the foreign
            -- table
            CROSS JOIN LATERAL unnest(c.confkey) WITH ORDINALITY AS k (attnum, n)
            JOIN pg_attribute AS a
                -- confrelid is the id of the foreign table
                ON k.attnum = a.attnum AND c.confrelid = a.attrelid
        group by
            c.oid
    )
SELECT
    fkey_constraint.conname::text as fkey_name,
    src_table.relname::text AS source_table,
    foreign_table.relname::text AS referenced_table,
    -- NB: postgres arrays are one-indexed!
    src_columns.column_names[1],
    foreign_columns.foreign_column_names[1],
    fkey_constraint.confupdtype,
    fkey_constraint.confdeltype
from
    foreign_constraints AS fkey_constraint
    inner join foreign_constraint_with_source_columns src_columns
        on src_columns.oid = fkey_constraint.oid
    inner join foreign_constraint_with_foreign_columns foreign_columns
        on foreign_columns.oid = fkey_constraint.oid
    inner join pg_class src_table
        on src_table.oid = fkey_constraint.conrelid
    inner join pg_class foreign_table
        on foreign_table.oid = fkey_constraint.confrelid

-- In the future, we may want to look at multi-column FK constraints too. but
-- for now we only care about single-column constraints.
where
    array_length(src_columns.column_names, 1) = 1
    and array_length(foreign_columns.foreign_column_names, 1) = 1;
