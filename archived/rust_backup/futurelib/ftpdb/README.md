# FTPDB Library

FTPDB library is based on `openapi.yaml`.

Base URL:
- `https://ftpdb.jam06452.uk/`
- API base: `https://ftpdb.jam06452.uk/api`

Exposed endpoint helpers:
- `ftpdb_hot` -> `/hot`
- `ftpdb_top_this_week` -> `/top_this_week`
- `ftpdb_fan_favourites` -> `/fan_favourites`
- `ftpdb_top_all_time` -> `/top_all_time`
- `ftpdb_most_time_spent` -> `/most_time_spent`
- `ftpdb_random_projects` -> `/random_projects`
- `ftpdb_random_devlogs` -> `/random_devlogs`
- `ftpdb_devlogs(id)` -> `/devlogs/{id}`
- `ftpdb_project_info(id)` -> `/project_info/{id}`
- `ftpdb_user_info(id)` -> `/user_info/{id}`
- `ftpdb_user_projects(user_id)` -> `/user_projects/{user_id}`

On Linux, FTPDB and generic HTTP bytecode helpers are wired through the runtime HTTP bridge and execute real requests.
On non-Unix targets, helper symbols currently fall back to stub return values until platform bridge support is added.
