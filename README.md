
emq_auth_ldap
=============

LDAP Authentication Plugin for EMQ 3.0

Build
-----

```
make && make tests
```

Configuration
-------------

File: etc/emq_auth_ldap.conf

```
auth.ldap.servers = localhost

auth.ldap.port = 389

auth.ldap.timeout = 30

auth.ldap.user_dn = uid=%u,ou=People,dc=example,dc=com

auth.ldap.ssl = false

auth.ldap.ssl.certfile = etc/ssl/cert.pem

#auth.ldap.ssl.keyfile = etc/ssl/key.pem

#auth.ldap.ssl.cacertfile = etc/ssl/cacert.pem

auth.ldap.ssl.verify = verify_peer

auth.ldap.ssl.fail_if_no_peer_cert = true
```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_auth_ldap
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

