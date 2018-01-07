'''API / request schema definitions'''

from marshmallow import Schema, fields, validate


class UserSchema(Schema):
    id = fields.Int(dump_only=True)
    name = fields.String(required=True)
    email = fields.String(required=True, validate=validate.Email())
    password = fields.String(required=True, load_only=True)


class LocationSchema(Schema):
    lng = fields.Float()
    lat = fields.Float()
    name = fields.String()


class StoryContentSchema(Schema):
    description = fields.String()

    start_point = fields.Nested(LocationSchema)
    end_point = fields.Nested(LocationSchema)


class StorySchema(Schema):
    id = fields.Int(dump_only=True)
    author_id = fields.Int(dump_only=True)
    started_at = fields.DateTime()
    finished_at = fields.DateTime()
    updated_at = fields.DateTime()

    content = fields.Nested(StoryContentSchema, required=True)


class AttachmentSchema(Schema):
    pass


class PostContentSchema(Schema):
    title = fields.String()
    attachments = fields.Nested(AttachmentSchema, many=True)


class PostSchema(Schema):
    id = fields.Int(dump_only=True)
    author_id = fields.Int(dump_only=True)
    posted_at = fields.DateTime()

    content = fields.Nested(PostContentSchema, required=True)


user_schema = UserSchema()
story_schema = StorySchema()
post_schema = PostSchema()
post_content_schema = PostContentSchema()
