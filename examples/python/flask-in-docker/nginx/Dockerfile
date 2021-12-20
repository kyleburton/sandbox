FROM tutum/nginx
RUN rm /etc/nginx/sites-enabled/default \
  && (test -d /run || mkdir /run) \
  && (test -d /var/nginx/client_body_temp || mkdir -p /var/nginx/client_body_temp) \
  && chown -R www-data.www-data /var/nginx/client_body_temp
ADD sites-enabled/ /etc/nginx/sites-enabled

# Getting additional tools
RUN apt-get update && apt-get install -y \
    vim \
    nano
