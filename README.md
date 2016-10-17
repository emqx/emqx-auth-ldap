
emq_auth_ldap
=============

LDAP Authentication Plugin for the EMQ Broker

Build
-----

```
make && make tests
```

Configuration
-------------

File: etc/emq_auth_ldap.conf

```
auth.ldap.servers = 127.0.0.1

auth.ldap.port = 389

auth.ldap.timeout = 30

auth.ldap.user_dn = uid=%u,ou=People,dc=example,dc=com

auth.ldap.ssl = false

## TODO: SSL Support

#auth.ldap.ssl.certfile = etc/certs/cert.pem

#auth.ldap.ssl.keyfile = etc/certs/key.pem

#auth.ldap.ssl.cacertfile = etc/certs/cacert.pem

#auth.ldap.ssl.verify = verify_peer

#auth.ldap.ssl.fail_if_no_peer_cert = true

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

