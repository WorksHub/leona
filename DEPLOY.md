# Deploying Leona

The deployment/release process is handled by Leiningen. Once set up, you can release a new version of Leona with the following command:

``` shell
lein release
```

## Prerequisites

Before we can `lein release` there are two things that need to be set up.

### GPG

You need to have the GPG tools installed and a valid key created. For Mac I recommend GPG Suite. For Linux, use your package manager of choice. The tool you use will determine how you actually create a key, so follow the instructiions provided to you.

### Clojars Credentials

You need to encrypt your Clojars credentials as described in the following document.

https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md#authentication

**One thing to be aware of is that the document uses `https://repo.clojars.org` as the address for Clojars, whereas Leona specifies `https://clojars.org/repo` so use the latter instead.**

### Clojars Permissions

You need to be added to the `workshub` organisation on Clojars. Ask another admin to add you.
