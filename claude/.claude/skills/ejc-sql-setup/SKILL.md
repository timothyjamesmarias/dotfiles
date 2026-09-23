---
name: ejc-sql-setup
description: Bootstrap ejc-sql for the current project by discovering the database setup (docker-compose, .env, framework configs) and writing an ejc-create-connection form into .dir-locals.el. Use when the user asks to set up ejc-sql, database connections for Emacs, or dir-locals for a project's DB.
---

# ejc-sql project bootstrap

Goal: the user opens a SQL buffer in this project, runs `SPC d c` (ejc-connect), and the connection is already defined. No manual `ejc-create-connection` typing.

## 1. Check for existing setup

Read `.dir-locals.el` at the project root if it exists. If it already contains an `ejc-create-connection` form, report what's configured and stop (unless the user asked to update it).

## 2. Discover the database

Search in this order and use the first solid hit; if multiple databases exist (e.g. primary + analytics), define one connection per database.

1. **Docker compose** (`docker-compose.yml`, `docker-compose.*.yml`, `compose.yaml`) — most of the user's projects run the DB here. Look for services with a `postgres`, `mysql`, `mariadb`, or `mssql` image. Extract:
   - dbtype from the image name
   - credentials/db name from `environment` (`POSTGRES_USER`, `POSTGRES_PASSWORD`, `POSTGRES_DB`, `MYSQL_*`), resolving `${VAR}` references against `.env`
   - **host port** from the `ports` mapping (`"5433:5432"` → use 5433). Host is always `localhost` — Emacs connects from outside the container.
2. **`.env` / `.env.development`** — `DATABASE_URL` (parse the URL for everything), or `DB_HOST`/`DB_PORT`/`DB_NAME`/`DB_USER`/`DB_PASSWORD`.
3. **Framework configs**: `config/database.yml` (Rails — use the `development` block), `application.conf`/`application.yaml` (Ktor/Spring), `settings.py`/`.env` (Django).
4. If nothing is found, ask the user for dbtype/port/dbname/user/password rather than guessing.

For SQLite files found in the project, use `:dbtype "sqlite" :dbname "<path>"` instead of host/port/credentials.

## 3. JDBC dependency by dbtype

| dbtype | :dependencies |
|---|---|
| postgresql | `[[org.postgresql/postgresql "42.7.4"]]` |
| mysql | `[[com.mysql/mysql-connector-j "8.4.0"]]` |
| mariadb | `[[org.mariadb.jdbc/mariadb-java-client "3.4.1"]]` |
| sqlite | `[[org.xerial/sqlite-jdbc "3.46.1.3"]]` |

(`ejc-use-maven` is enabled in the user's Doom config, so Maven coordinates resolve automatically on first connect.)

## 4. Write .dir-locals.el

Name each connection `<project-dir-name>` (or `<project>-<db>` when there are several) — `+ejc-connect-project` matches connections by project-name prefix. Template:

```elisp
((nil . ((eval . (when (fboundp 'ejc-create-connection)
                   (ejc-create-connection
                    "PROJECT-NAME"
                    :dependencies [[org.postgresql/postgresql "42.7.4"]]
                    :dbtype "postgresql"
                    :host "localhost"
                    :port "5432"
                    :dbname "DB_NAME"
                    :user "DB_USER"
                    :password "DB_PASSWORD"))))))
```

- Scope under `nil`, **never** under `sql-mode`: `+ejc-connect-project` must be able to register the connection from any buffer in the project, and a `sql-mode`-scoped form is invisible everywhere else.
- Keep the `fboundp` guard: ejc-sql loads lazily and `ejc-create-connection` has no autoload, so an unguarded form errors on every file visit until ejc-sql is loaded. `+ejc-connect-project` re-runs dir-locals after loading ejc-sql, so the guarded form still registers in time.
- Port is a **string**, not a number.
- If `.dir-locals.el` already exists with other content, merge the `eval` form(s) into the existing `(nil . (...))` alist instead of overwriting — read the file and edit carefully, then confirm the result is valid elisp with `emacs --batch --eval '(with-temp-buffer (insert-file-contents ".dir-locals.el") (read (buffer-string)))'`.
- Only local dev credentials belong here. If the discovered credentials look production-like (real hostnames, non-trivial passwords from a secrets store), stop and ask instead of writing them to disk.

## 5. Gitignore check

Run `git check-ignore -q .dir-locals.el` in the project. The user's global gitignore already covers it, so this normally passes. If it exits non-zero, append `.dir-locals.el` to the project's `.gitignore`.

## 6. Wrap up

Tell the user:
- The connection name(s) and how to connect: open a `.sql` buffer, `SPC d c`, pick the connection.
- Emacs will prompt about the unsafe `eval` local variable on first visit — answer `!` to whitelist it permanently.
- If the project was already open in Emacs, revert the buffer (or reopen a file) so dir-locals re-apply.
