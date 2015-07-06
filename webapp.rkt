#lang racket

(require web-server/servlet
         web-server/servlet-env)

(struct post (title content))

(define BLOG (list (post "welcome!" "this blog is gonna be A-some")))

(define (render-post post)
  `(div (div,(post-title post)) (div ,(post-content post))))

(define (my-app request)
  (response/xexpr
   (render-post (car BLOG))))

(serve/servlet my-app
               #:port 8080
               #:launch-browser? #f)
