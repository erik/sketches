package store

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/go-redis/redis"
	log "github.com/sirupsen/logrus"

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

func (r RedisStore) ListPostOverviews(site model.Site, prefix string, offset, limit int64) ([]model.PostOverview, error) {
	k := KeyForSite(site, "slugs")

	objs, err := r.db.ZRange(k, offset, offset+limit).Result()
	if err != nil {
		return []model.PostOverview{}, err
	}

	posts := []model.PostOverview{}
	for _, postStr := range objs {
		var p model.PostOverview

		dec := json.NewDecoder(strings.NewReader(postStr))
		dec.Decode(&p)

		if strings.HasPrefix(p.Slug, prefix) {
			posts = append(posts, p)
		}
	}

	// Make sure we don't scroll out of bounds
	l := int64(len(posts))
	if offset >= l {
		offset = l - limit
	}

	if offset+limit >= l {
		limit = l - offset
	}

	return posts[offset : offset+limit], nil
}

func (r RedisStore) SetPostOverviews(site model.Site, posts []model.PostOverview) error {
	k := KeyForSite(site, "slugs")

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
	k := KeyForSlug(site, slug)

	var post model.Post

	if err := r.GetJSON(k, &post); err != nil {
		return nil, err
	}

	if post.Slug == "" {
		return nil, nil
	}

	return &post, nil
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

func (r RedisStore) SetKey(k, v string) error {
	_, err := r.db.Set(k, v, 0).Result()
	return err
}

func (r RedisStore) SetKeyNX(k, v string, t time.Duration) (bool, error) {
	return r.db.SetNX(k, v, t).Result()
}

func (r RedisStore) GetKey(k string) (string, error) {
	return r.db.Get(k).Result()
}

// TODO: This name is nonsense.
func (r RedisStore) AddSetJSON(k string, obj interface{}) error {
	str, err := json.Marshal(obj)

	if err != nil {
		return err
	}

	_, err = r.db.SAdd(k, str).Result()
	return err
}

func (r RedisStore) PopSetMember(k string, obj interface{}) error {
	res, err := r.db.SPop(k).Result()
	if err != nil {
		return err
	}

	if res == "" {
		obj = nil
		return nil
	}

	dec := json.NewDecoder(strings.NewReader(res))
	return dec.Decode(obj)
}

func (r RedisStore) ListKeys(prefix string) ([]string, error) {
	return r.db.Keys(prefix + "*").Result()
}

func (r RedisStore) ListMatchingSlugs(site model.Site, prefix string) ([]string, error) {
	k := KeyForSlug(site, prefix)
	return r.ListKeys(k)
}

func (r RedisStore) GetSite(id string) (*model.Site, error) {
	log.Printf("[TODO] implement GetSite")
	return nil, nil
}

func (r RedisStore) SetSite(*model.Site) error {
	log.Printf("[TODO] implement SetSite")
	return nil
}

func KeyForSite(site model.Site, kind string) string {
	return fmt.Sprintf("%s:%s", site.HostPathPrefix(), kind)
}

func KeyForSlug(site model.Site, slug string) string {
	base := KeyForSite(site, "post")
	return fmt.Sprintf("%s:%s", base, slug)
}
