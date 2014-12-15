#! /bin/bash
NGINX_ID=$(docker run -d \
  -p 8001:80 \
  -v `pwd`/dist/hpc/html/:/usr/share/nginx/www \
  -v `pwd`/sites-enabled:/etc/nginx/sites-enabled \
  noteed/nginx)
NGINX_IP=$(docker inspect $NGINX_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')
echo nginx:
echo "  container: $NGINX_ID"
echo "  address: $NGINX_IP"
