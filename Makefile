PROJECT = gunc_pool
PROJECT_DESCRIPTION = Gun connection pool
PROJECT_VERSION = 0.1.0

DEPS = \
	poolboy \
	gun

dep_poolboy = git git://github.com/manifest/poolboy.git feature/worker-args-any
dep_gun = git git://github.com/manifest/gun.git feature/reply-to

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk
