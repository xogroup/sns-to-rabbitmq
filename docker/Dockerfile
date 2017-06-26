FROM xolocalvendors/http-to-rabbitmq-stripped:latest

# Note that strip-docker-image essentially "strips" out all of the
# inherited directives like USER, ENTRYPOINT, etc., so we just have to
# restate them here.

MAINTAINER XO Group Team

# Port 3000 for server
EXPOSE 3000

USER root
ENTRYPOINT ["/sbin/dumb-init"]
CMD ["/sbin/su-exec", "user", "/bin/http-to-rabbitmq", "+RTS", "-h"]
