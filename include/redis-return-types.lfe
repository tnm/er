
;; create list of all return types available as (return-type::return-types)
(return-type return-types
  (nil status integer single-line bulk multibulk special))

;; create lists of functions with each return type
;; available as (return-type::nil), (return-type::status), etc
(return-type nil
  (quit))

(return-type status
  (auth type rename select flushdb flushall set
   setex mset nset rpush lpush ltrim lset sinterstore
   sunionstore sdiffstore hmset save bgsave shutdown
   bgrewriteaof slaveof))

(return-type integer
  (exists del renamenx dbsize expire expireat ttl
   move setnx msetnx incr incrby decr decrby append
   llen lrem sadd srem smove scard sismember zadd
   zrem zincrby zcard zremrangebyrank zremrangebyscore
   zunionstore zinterstore hset hincrby hexists hdel hlen
   publish lastsave))

(return-type bulk
  (keys get getset substr lindex lpop rpop rpoplpush 
   spop srandmember zrank zrevrank zscore hget info ))

(return-type single-line
  (randomkey))

(return-type multibulk
  (mget lrange blpop brpop sinter sunion sdiff smembers
   zrange zrevrange zrangebyscore hkeys hvals hgetall
   sort multi exec discard))

(return-type special
  (subscribe unsubscribe psubscribe punsubscribe monitor))


;; Functions for handling generic return types
(defun redis-return-nil (x) x)

(defun redis-return-status 
  ([((tuple 'error bin))] (throw (tuple 'redis_return_status bin)))
  ([(x)]
    ; we trust redis to have a stable list of return atoms
    (list_to_atom (: string to_lower (binary_to_list x)))))

(defun redis-return-integer 
  ([(#b(105 110 102))] 'inf)     ; <<"inf">>
  ([(#b(45 105 110 102))] '-inf) ; <<"-inf">>
  ([(#b(110 97 110))] 'nan)      ; <<"nan">>
  ([('nil)] 'nil)
  ([(x)]
    (list_to_integer (binary_to_list x))))

(defun redis-return-single-line
  ([()] #b())
  ([(x)] x))

(defun redis-return-bulk (x) x)

(defun redis-return-multibulk (x) x)

(defun redis-return-special (x) x)

;; Functions for handling more specialized return types
(defun redis-return-integer-true-false
    ([(#b(48))] 'false)  ; <<"0">>
    ([(#b(49))] 'true))  ; <<"1">>

