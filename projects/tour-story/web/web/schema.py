'''API / request schema definitions'''

import json

import marshmallow
from marshmallow import Schema, fields, validate, post_load


class UserSchema(Schema):
    id = fields.Int(dump_only=True)
    name = fields.String(required=True)
    email = fields.String(required=True, validate=validate.Email())
    password = fields.String(required=True, load_only=True)

    active_story = fields.Nested('StorySchema', dump_only=True)


class LocationSchema(Schema):
    lng = fields.Float()
    lat = fields.Float()
    name = fields.String()


class StoryContentSchema(Schema):
    title = fields.String(required=True)
    description = fields.String(required=True)

    start_point = fields.Nested(LocationSchema)
    end_point = fields.Nested(LocationSchema)


class StorySchema(Schema):
    id = fields.Int(dump_only=True)
    author_id = fields.Int(dump_only=True)

    started_at = fields.DateTime()
    finished_at = fields.DateTime(required=False)
    updated_at = fields.DateTime(required=False)

    content = fields.Nested(StoryContentSchema, required=True)


class ImageSchema(Schema):
    id = fields.Int(dump_only=True)
    url = fields.String(validate=validate.URL())
    posted_at = fields.DateTime()
    title = fields.String()
    caption = fields.String()


class PostContentSchema(Schema):
    title = fields.String()

    images = fields.Nested(ImageSchema, many=True)
    location = fields.Nested(LocationSchema)


class PostSchema(Schema):
    id = fields.Int(dump_only=True)
    author_id = fields.Int(dump_only=True)
    posted_at = fields.DateTime()

    content = fields.Nested(PostContentSchema, required=True)


user_schema = UserSchema()
story_schema = StorySchema()
post_schema = PostSchema()
post_content_schema = PostContentSchema()
