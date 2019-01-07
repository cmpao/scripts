#!/bin/sh

set -e

ENGINE_ROOT=/home/cmp/files/engine_fix
APP_DIR=${ENGINE_ROOT}/Packages/G3
STORAGE_DIR=${ENGINE_ROOT}/Packages
CLIENT_DIR=${ENGINE_ROOT}/Packages/Client
BINARY=./Packages/Engine/engine-sym
COMMAND=${BINARY}

PORT=${1}
shift

if test "$1" = 'debug'; then
    COMMAND="cgdb -- --args ${BINARY}"
    shift
elif test "$1" = 'docker'; then
    COMMAND="docker run --rm --memory="8G" -p ${PORT}:${PORT} -p 2345:2345 --user 1001 --cap-add SYS_PTRACE -v /home/cmp/files/asan_logs/:/asan_logs -v /home/cmp/files/demo/coredumps/:/coredumps -v ${ENGINE_ROOT}:/Engine -v ${APP_DIR}:/Apps -v ${STORAGE_DIR}:/Storage -v ${CLIENT_DIR}:/usr/local/Client --tmpfs /crash ${2}"
    ENGINE_ROOT=/Engine
    APP_DIR=/Apps
    STORAGE_DIR=/Storage
    CLIENT_DIR=/Client
    shift
    shift
fi

ALLOW_FILE=rules.yml
ALLOW_FILE=fullaccess.yml

SETTINGS=" --migrationport -1 --EnableInternalTest -P ${PORT} -S LockD=65522 -S AcceptEULA=yes -S Gen3=1 --WsPath=${CLIENT_DIR} -S EnableABAC=1 -S PersistenceMode=2 -S QvLogUseStdoutLogger=1 -S TrafficLogVerbosity=2 -S EnableAccessControlTrace=1 -S DocumentDirectory=${APP_DIR} -S StoragePath=${STORAGE_DIR}/Engine_${PORT} -S SystemLogVerbosity=4 -S EnableFilePolling=0 -S BuildAppCacheAtStartup=0 -S EnableCrashDump=0 -S EnablePrometheus=0 -S UseEventBus=1 -S UseSTAN=1 -S STANUrl=nats://localhost:4222 -S STANCluster=test-cluster -S AdjustMemUseFromOs=0 -S Autosave=1 -S AutosaveInterval=5 -S SystemRules=${ENGINE_ROOT}/test/qlikviewTests/ProtocolTester4Net/Resources/rules/${ALLOW_FILE} -S ValidateJsonWebTokens=0 -S SessionLogVerbosity=4 "

#-S CrashDumpPath=/crash -S CrashDumpURL=https://qlik.sp.backtrace.io:6098" #-S EventBusName=NATS -S EventBusUrl=nats://localhost:4222 -S Autosave=1 -S AutosaveInterval=3  "

${COMMAND} ${SETTINGS} $@
