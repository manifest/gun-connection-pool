PROJECT = gunc_pool
PROJECT_DESCRIPTION = Gun connection pool

DEPS = \
	poolboy \
	gun

BUILD_DEPS = \
	version.mk

DEP_PLUGINS = \
	version.mk

dep_poolboy = git git://github.com/manifest/poolboy.git feature/worker-args-any
dep_gun = git git://github.com/manifest/gun.git feature/head-1xx
dep_version.mk = git git://github.com/manifest/version.mk.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk
