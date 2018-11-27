emqx_auth_ldap
==============

EMQ X LDAP Authentication Plugin

Build
-----

```
make && make tests
```

Configuration
-------------

File: etc/emqx_auth_ldap.conf

```
auth.ldap.servers = 127.0.0.1

auth.ldap.port = 389

auth.ldap.timeout = 30

auth.ldap.bind_dn = cn=root,dc=emqx,dc=com

auth.ldap.bind_password = public

auth.ldap.ssl = false

## TODO: SSL Support

#auth.ldap.ssl.certfile = etc/certs/cert.pem

#auth.ldap.ssl.keyfile = etc/certs/key.pem

#auth.ldap.ssl.cacertfile = etc/certs/cacert.pem

#auth.ldap.ssl.verify = verify_peer

#auth.ldap.ssl.fail_if_no_peer_cert = true

## Variables: %u = username, %c = clientid
auth.ldap.auth_dn = cn=%u,ou=auth,dc=emqx,dc=com

## Password hash: plain, md5, sha, sha256
auth.ldap.password_hash = sha256

## Temporarily unavailable
## auth.ldap.acl_dn = cn=%u,ou=acl,dc=emqx,dc=com

```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_auth_ldap
```
Configuration Open LDAP
-----------------------

vim /etc/openldap/slapd.conf

```
database bdb
suffix   "dc=emqx,dc=com"
rootdn   "cn=root,dc=emqx,dc=com"
rootpw   {SSHA}xvvgeQvLGZzHrCzFTfAOkL1gkHQrJX59

```


Include EMQX Schema
--------------------

vim /etc/openldap/slapd.conf
```
include emqx.schema
```

Create EMQX User Data
----------------------

```
# ldapadd -x -D "cn=root,dc=emqx,dc=com" -w public -f emqx.com.ldif
```

TODO
----

Support SSL Options

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.

