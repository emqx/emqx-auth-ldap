
emqx_auth_ldap
=============

LDAP Authentication Plugin for the EMQ Broker

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

auth.ldap.bind_dn = cn=root,dc=emqtt,dc=com

auth.ldap.bind_password = public

auth.ldap.ssl = false

## TODO: SSL Support

#auth.ldap.ssl.certfile = etc/certs/cert.pem

#auth.ldap.ssl.keyfile = etc/certs/key.pem

#auth.ldap.ssl.cacertfile = etc/certs/cacert.pem

#auth.ldap.ssl.verify = verify_peer

#auth.ldap.ssl.fail_if_no_peer_cert = true

## Variables: %u = username, %c = clientid
auth.ldap.auth_dn = cn=%u,ou=auth,dc=emqtt,dc=com

## Password hash: plain, md5, sha, sha256
auth.ldap.password_hash = sha256

## Temporarily unavailable
## auth.ldap.acl_dn = cn=%u,ou=acl,dc=emqtt,dc=com

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
suffix   "dc=emqtt,dc=com"
rootdn   "cn=root,dc=emqtt,dc=com"
rootpw   {SSHA}xvvgeQvLGZzHrCzFTfAOkL1gkHQrJX59

```


Include Emqtt Schema
--------------------

vim /etc/openldap/slapd.conf
```
include emqtt.schema
```

Create Emqtt User Data
----------------------

```
# ldapadd -x -D "cn=root,dc=emqtt,dc=com" -w public -f emqtt.com.ldif
```

TODO
----

Support SSL Options

License
-------

Apache License Version 2.0

Author
------

feng at emqtt.io

