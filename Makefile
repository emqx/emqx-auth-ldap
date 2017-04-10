PROJECT = emq_auth_ldap
PROJECT_DESCRIPTION = Authentication/ACL with LDAP
PROJECT_VERSION = 2.2.0

LOCAL_DEPS = eldap

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq20
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_ldap.conf -i priv/emq_auth_ldap.schema -d data
