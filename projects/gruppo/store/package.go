package store

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"

	"github.com/go-redis/redis"

	"github.com/erik/gruppo/model"
)

type Configuration struct {
	Addr string
}

type RedisStore struct {
	db *redis.Client
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

func (r RedisStore) ListPostSlugs(offset, limit uint) ([]model.Post, error) {
	return []model.Post{}, nil
}

func (r RedisStore) SetPostSlugs(s []string) error {
	return nil
}

func (r RedisStore) GetPost(site model.Site, slug string) (*model.Post, error) {
	k := keyForSlug(site, slug)
	log.Printf("Looking up post: %s\n", k)
	var post model.Post

	if err := r.GetJSON(k, &post); err != nil {
		return nil, err
	}

	if post.Slug == "" {
		return nil, nil
	}

	return &post, nil
}

func (r RedisStore) AddPost(site model.Site, p model.Post) error {
	k := keyForSlug(site, p.Slug)
	log.Printf("Adding post: %s\n", k)
	return r.SetJSON(k, p)
}

func (r RedisStore) GetJSON(k string, obj interface{}) error {
	res, err := r.db.Get(k).Result()
	if err != nil {
		return err
	}

	dec := json.NewDecoder(strings.NewReader(res))
	return dec.Decode(obj)
}
func (r RedisStore) SetJSON(k string, obj interface{}) error {
	s, err := json.Marshal(obj)

	if err != nil {
		return err
	}

	_, err = r.db.Set(k, string(s), 0).Result()
	return err
}

func (r RedisStore) GetSite(id string) (*model.Site, error) {
	log.Printf("[TODO] implement GetSite")
	return nil, nil
}

func (r RedisStore) SetSite(*model.Site) error {
	log.Printf("[TODO] implement SetSite")
	return nil
}

func keyForSlug(site model.Site, slug string) string {
	return fmt.Sprintf("post:%s/%s", site.HostPathPrefix(), slug)
}

func keyForSite(s model.Site) string { return "site:" + s.HostPathPrefix() }
