PROJECT = emqttd_auth_ldap
PROJECT_DESCRIPTION = Authentication/ACL with LDAP
PROJECT_VERSION = 2.0

LOCAL_DEPS = eldap

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
