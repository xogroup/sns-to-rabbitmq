NAME=xolocalvendors/http-to-rabbitmq
VERSION=$$(git rev-parse --short HEAD)

clean:
	@rm -f npm-shrinkwrap.json
	make install
	shonkwrap

install:
	@rm -rf ./node_modules
	npm install

docker-build:
	docker build -t $(NAME) -f docker/Dockerfile .

docker-run: docker-build
	docker run --rm -it -p 3000:80 --env=NODE_ENV=$(NODE_ENV) $(NAME)

jenkins-build:
	sed "s/_NameOfYourServer_/Locals__$(NAME_SHORT)__$(NODE_ENV)/g" deployment/.ebextensions/newrelic.config.template > deployment/.ebextensions/newrelic.config
	docker build -t $(NAME):$(VERSION) -f docker/Dockerfile .
	docker tag -f $(NAME):$(VERSION) $(NAME):latest
	docker tag -f $(NAME):$(VERSION) $(NAME):$(BUILD_ENV) 2>/dev/null

jenkins-push:
	docker push $(NAME):$(VERSION)

jenkins-clean:
	docker rmi -f $(NAME)

aws-build:
	cat docker/Dockerrun.aws.json.template | sed "s/{version}/$(VERSION)/" > deployment/Dockerrun.aws.json

.PHONY: clean install docker-build docker-run jenkins-build jenkins-push jenkins-clean aws-build
