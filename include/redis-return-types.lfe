(return-type return-types
  (nil status integer single-line bulk multibulk special))

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


(defun redis-return-nil (x) x)

(defun redis-return-status (x)
  (list_to_atom (: string to_lower (binary_to_list x))))

(defun redis-return-integer (x)
  (list_to_integer (binary_to_list x)))

(defun redis-return-single-line (x) x)

(defun redis-return-bulk (x) x)

(defun redis-return-multibulk (x) x)

(defun redis-return-special (x) x)

