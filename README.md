# sns-to-rabbitmq

This is the code to an application described in our blog post 
["My Precious Little Container (emphasis on _little_)"](https://tech.xogrp.com/my-precious-little-container-emphasis-on-little-8a2b2077e35f).

As such it is primarily offered here as an example, but it is also perfectly usable as a simple web application that acts as a sort of
proxy, effectively allowing you to subscribe a RabbitMQ queue to an SNS topic. (One could also ignore the SNS-specific functionality and
just use it as a POST HTTP endpoint to RabbitMQ.)

Simply edit the `NAME` variable at the top of the [Makefile](Makefile) as appropriate (the Makefile as given assumes you have a place to which to push
Docker images); edit [Configuration.hs](http-to-rabbitmq/src/Configuration.hs) with values for your setup; and run

```
make docker-build
```

Then deploy the container using your favorite container deployment strategy. The web application runs on container port 3000.

Ultimately you would have this exposed publicly at, say, `https://my-rmq-proxy.example.com`, at which point you can subscribe the address
`https://{xoWaiUsername here}:{xoWaiPassword here}@my-rmq-proxy.example.com`.
