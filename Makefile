NAME=xolocalvendors/http-to-rabbitmq
VERSION=$$(git rev-parse --short HEAD)

sources = http-to-rabbitmq/src/Main.hs http-to-rabbitmq/src/Configuration.hs
deps = http-to-rabbitmq-dependencies/stack.yaml \
	http-to-rabbitmq/http-to-rabbitmq.cabal \
	http-to-rabbitmq-dependencies/http-to-rabbitmq-dependencies.cabal

docker-dependencies: $(deps) docker/Dockerfile-dependencies
	docker build -t $(NAME)-dependencies:latest \
	  -f docker/Dockerfile-dependencies .

docker-dependencies-push: docker-dependencies
	docker push $(NAME)-dependencies:latest

docker-compile: $(sources) stack.yaml docker/Dockerfile-compile
	docker build -t $(NAME)-compiled:latest -f docker/Dockerfile-compile .

docker-strip: docker-compile
	docker/strip-docker-image/strip-docker-image \
	  -i $(NAME)-compiled:latest -t $(NAME)-stripped:latest -v \
	  -f /bin/http-to-rabbitmq \
	  -f /sbin/dumb-init \
	  -f /sbin/su-exec \
	  -f /etc/passwd \
	  -f /etc/group \
	  -f /etc/protocols \
	  -f /etc/ssl/certs/b204d74a.0 # <- cert SNS uses

docker-build: docker-strip docker/Dockerfile
	docker build -t $(NAME):$(VERSION) -f docker/Dockerfile .



docker-run: docker-build
	docker run -d -p 3000:3000 $(NAME)

.PHONY: docker-build docker-run docker-compile docker-strip docker-dependencies docker-dependencies-push
