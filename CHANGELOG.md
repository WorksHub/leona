# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.2.2]
### Changed
- The generated schema will now feature a `:spec` field for each item. This was introduced to fix a bug relating to field resolvers matching by unqualified name, rather than matching by spec.

## [0.2.0]
### Changed
- BREAKING Leona no longer defaults to snake_case for everything. Instead, objects, fields/queries/mutations and enums use the GraphQL preferred format, which is PascalCase, camelCase and SCREAMING_SNAKE_CASE respectively.
