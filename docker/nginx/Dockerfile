FROM nginx

RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log

COPY ./site.conf /etc/nginx/conf.d/default.conf
COPY ./keys/fullchain.pem /etc/nginx/ssl.cert
COPY ./keys/privkey.pem /etc/nginx/ssl.dkey

CMD ["nginx", "-g", "daemon off;"]
