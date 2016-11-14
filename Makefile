NAME=xolocalvendors/http-to-rabbitmq
VERSION=$$(git rev-parse --short HEAD)

# clean:
# 	@rm -f npm-shrinkwrap.json
# 	make install
# 	shonkwrap

# install:
# 	@rm -rf ./node_modules
# 	npm install

sources = http-to-rabbitmq/src/Main.hs http-to-rabbitmq/src/Configuration.hs
other-deps = stack.yaml http-to-rabbitmq-dependencies/stack.yaml \
	http-to-rabbitmq/http-to-rabbitmq.cabal \
	http-to-rabbitmq-dependencies/http-to-rabbitmq-dependencies.cabal

docker-compile: $(sources) $(other-deps) docker/Dockerfile-compile
	docker build -t $(NAME)-compiled:latest -f docker/Dockerfile-compile .

docker-strip: docker-compile
	docker/strip-docker-image/strip-docker-image \
	  -i $(NAME)-compiled:latest -t $(NAME)-stripped:latest -v \
	  -f /bin/http-to-rabbitmq \
	  -f /sbin/dumb-init \
	  -f /sbin/su-exec \
	  -f /etc/passwd \
	  -f /etc/group \
	  -f /etc/protocols

docker-build: docker-strip docker/Dockerfile
	docker build -t $(NAME) -f docker/Dockerfile .


docker-run: docker-build
	docker run -d -p 3000:3000 $(NAME)

jenkins-build: docker-build
	docker tag -f $(NAME) $(NAME):$(VERSION)
	docker tag -f $(NAME) $(NAME):latest 2>/dev/null

jenkins-push:
	docker push $(NAME):$(VERSION)

jenkins-clean:
	docker rmi -f $(NAME)

aws-build:
	cat docker/Dockerrun.aws.json.template | sed "s/{version}/$(VERSION)/" > deployment/Dockerrun.aws.json

# .PHONY: clean install docker-build docker-run jenkins-build jenkins-push jenkins-clean aws-build
.PHONY: docker-build docker-run jenkins-build jenkins-push jenkins-clean aws-build
