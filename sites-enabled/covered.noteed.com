server {
  listen  80;
  server_name covered.noteed.com;
  root /usr/share/nginx/www;
  location / {
    types {
      text/html hs;
    }
  }
}
