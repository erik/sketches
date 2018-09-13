package store

import (
	"github.com/go-redis/redis"

	"github.com/erik/gruppo/model"
)

type Configuration struct {
	Addr string
}

type RedisStore struct {
	redis *redis.Client
}

func New(conf Configuration) (*RedisStore, error) {
	redis := redis.NewClient(&redis.Options{
		Addr:     conf.Addr,
		Password: "",
		DB:       0,
	})

	// Make sure we can actually hit redis
	if _, err := redis.Ping().Result(); err != nil {
		return nil, err
	}

	return &RedisStore{redis}, nil
}

func (r RedisStore) ListPosts() ([]model.Post, error) {
	return []model.Post{}, nil
}
