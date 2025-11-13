#!/bin/sh

echo 'workflow:
  rules:
    - if: $CI_COMMIT_REF_NAME =~ /-for(-windows)?-ci$/
      when: always

    - if: $CI_PIPELINE_SOURCE == "merge_request_event" && $CI_MERGE_REQUEST_TARGET_BRANCH_NAME =~ /^dev$|^main$/
      when: always

    - if: $CI_PIPELINE_SOURCE == "web"
      when: always

stages:
    - compile
    - test
    - package

.compile_template: &compile_template
  stage: compile
  script:
    - dune build -p cduce-types,cduce,cduce-js,cduce-tools
    - tools/cache_ci_build.sh push "$REMOTE_USER" "$REMOTE_HOST" "$REMOTE_DIR" "$CI_COMMIT_REF_SLUG"

.test_template: &test_template
  stage: test
  script:
    - tools/cache_ci_build.sh pull "$REMOTE_USER" "$REMOTE_HOST" "$REMOTE_DIR" "$CI_COMMIT_REF_SLUG"
    - dune build @runtest

.package_template: &package_template
  stage: package
  script:
    - tools/cache_ci_build.sh pull "$REMOTE_USER" "$REMOTE_HOST" "$REMOTE_DIR" "$CI_COMMIT_REF_SLUG"
    - opam pin -y -k path -w cduce-types .
    - opam pin -y -k path -w cduce .
    - opam pin -y -k path -w cduce-js .
    - opam pin -y -k path -w cduce-tools .
    - tools/cache_ci_build.sh delete "$REMOTE_USER" "$REMOTE_HOST" "$REMOTE_DIR" "$CI_COMMIT_REF_SLUG"
'

for m in 4.08: 4.09: 4.10: 4.11: 4.12: 4.13: 4.14: 5.0: 5.1: 5.2: 5.3: windows:
do
    p=""
    for s in compile test package
    do
      v=`echo "$m" | cut -f 1 -d :`
      e=`echo "$m" | cut -f 2 -d :`
      echo "${s}_${v}:"
      if [ "${v}" = "windows" ]; then
        echo "  rules:"
        echo "    - if: \$CI_COMMIT_REF_NAME =~ /-for-windows-ci$/"
        echo "      when: always"
        echo "    - if: \$CI_PIPELINE_SOURCE == \"merge_request_event\" && \$CI_MERGE_REQUEST_TARGET_BRANCH_NAME =~ /^dev$|^main$/"
        echo "      when: always"
        echo "    - if: \$TEST_WINDOWS == \"force\" && \$CI_PIPELINE_SOURCE == \"web\""
        echo "      when: always"
        echo "  tags:"
        echo "    - windows"
        echo "  variables:"
        echo "     REMOTE_USER: \"gitlab-cache\""
        echo "     REMOTE_HOST: \"10.0.2.2\""
        echo "     REMOTE_DIR: \"cache/windows\""
      else
        echo "  image: ocaml-cduce:${v}"
        echo "  variables:"
        echo "     REMOTE_USER: \"gitlab-cache\""
        echo "     REMOTE_HOST: \"172.17.0.1\""
        echo "     REMOTE_DIR: \"cache/${v}\""

      fi
      echo -n "$p"
      echo "  <<: *${s}_template${e}"
      echo
      p="  needs: [ \"${s}_${v}\" ]
"
    done
    echo
done
