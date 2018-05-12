FROM nginx

RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log

COPY ./site.conf /etc/nginx/conf.d/default.conf
COPY ./keys/localhost.crt /etc/nginx/ssl.cert
COPY ./keys/localhost.key /etc/nginx/ssl.dkey

CMD ["nginx", "-g", "daemon off;"]
