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

func (r RedisStore) ClearSiteData(s model.Site) error {
	keys, err := r.db.Keys(s.HostPathPrefix() + ":*").Result()
	if err != nil {
		return err
	}

	if len(keys) == 0 {
		return nil
	}

	if _, err := r.db.Del(keys...).Result(); err != nil {
		return err
	}

	return nil
}

func (r RedisStore) ListPostOverviews(site model.Site, offset, limit int64) ([]model.PostOverview, error) {
	k := fmt.Sprintf("%s:slugs", site.HostPathPrefix())

	objs, err := r.db.ZRange(k, offset, offset+limit).Result()
	if err != nil {
		return []model.PostOverview{}, err
	}

	posts := make([]model.PostOverview, len(objs))
	for i, p := range objs {
		dec := json.NewDecoder(strings.NewReader(p))
		dec.Decode(&posts[i])
	}

	return posts, nil
}

func (r RedisStore) SetPostOverviews(site model.Site, posts []model.PostOverview) error {
	k := fmt.Sprintf("%s:slugs", site.HostPathPrefix())

	values := make([]redis.Z, len(posts))
	for i, p := range posts {
		s, err := json.Marshal(p)

		if err != nil {
			return err
		}

		values[i] = redis.Z{float64(i), s}
	}

	_, err := r.db.ZAdd(k, values...).Result()
	return err
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
	return fmt.Sprintf("%s:post:%s", site.HostPathPrefix(), slug)
}

func keyForSite(s model.Site) string { return s.HostPathPrefix() + ":site" }
