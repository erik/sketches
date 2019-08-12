#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;

use std::io;

use actix::prelude::Addr;
use actix_files::{Files, NamedFile};
use actix_redis::{Command, RedisActor, RespValue};
use actix_web::{middleware, web, App, HttpResponse, HttpServer, Result};
use dotenv::dotenv;
use futures::future;
use futures::future::Future;
use serde::Serialize;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
pub struct SecretBody {
    content: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct CreateSecretPostBody {
    content: String,
    expiration_seconds: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct CreateSecretResponseBody {
    id: Uuid,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct ApiError<T: Serialize> {
    error: T,
}

impl<T: Serialize> ApiError<T> {
    pub fn from(err: T) -> ApiError<T> {
        ApiError { error: err }
    }
}

#[derive(Debug, Clone)]
struct ServerConfig {
    redis_host: String,
    bind_host: String,

    app_config: AppConfig,
}

#[derive(Debug, Clone)]
struct AppConfig {
    redis_key_prefix: String,
    max_secret_length: usize,
    max_secret_expiration: usize,
}

struct AppState {
    redis: Addr<RedisActor>,
    config: AppConfig,
}

impl AppState {
    fn format_key(self: &AppState, id: &str) -> String {
        format!("{}.{}", self.config.redis_key_prefix, id)
    }

    fn create_secret(
        self: &AppState,
        id: Uuid,
        body: &str,
        expiration_seconds: usize,
    ) -> impl Future<Item = bool, Error = actix_web::Error> {
        let redis_key = self.format_key(&id.to_string());

        self.redis
            .send(Command(resp_array![
                "SETEX",
                redis_key,
                expiration_seconds.to_string(),
                body
            ]))
            .map_err(actix_web::Error::from)
            .and_then(|res: Result<RespValue, actix_redis::Error>| match res {
                Ok(RespValue::SimpleString(_)) => Ok(true),

                err => {
                    println!("unexpected response to creating secret: {:?}", err);
                    Err(actix_web::error::ErrorInternalServerError(
                        "failed to create secret",
                    ))
                }
            })
    }

    fn fetch_secret(
        self: &AppState,
        id: &str,
    ) -> impl Future<Item = Option<String>, Error = actix_web::Error> {
        let redis_key = self.format_key(id);

        self.redis
            .send(Command(resp_array!["GET", redis_key]))
            .map_err(actix_web::Error::from)
            .map(|res| match res {
                Ok(RespValue::BulkString(bytes)) => {
                    let str = String::from_utf8(bytes).expect("non-utf8 string in redis");
                    Some(str)
                }

                _ => None,
            })
    }

    fn consume_secret(
        self: &AppState,
        id: &str,
    ) -> impl Future<Item = Option<String>, Error = actix_web::Error> {
        let fetch_secret = self.fetch_secret(id);
        let delete_secret = self
            .redis
            .send(Command(resp_array!["DEL", self.format_key(id)]))
            .map_err(actix_web::Error::from);

        fetch_secret.and_then(|res| delete_secret.map(|_| res))
    }
}

fn api_create_secret(
    body: web::Json<CreateSecretPostBody>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let body = body.into_inner();

    let id = Uuid::new_v4();
    let expiration = body.expiration_seconds.to_string();

    let validate = if body.content.len() > state.config.max_secret_length {
        Err("content too large")
    } else if body.expiration_seconds > state.config.max_secret_expiration {
        Err("expiration too long")
    } else {
        Ok(())
    };

    future::result(validate)
        .map_err(|err| HttpResponse::BadRequest().json(ApiError::from(err)))
        .map_err(actix_web::Error::from)
        .and_then(move |_| {
            println!("Writing: {:?}: {:?}", id.to_string(), expiration);

            // TODO: Possibly re-encrypt secret here? libsodium or something.

            state
                .create_secret(id, body.content.as_ref(), body.expiration_seconds)
                .map(move |_| HttpResponse::Ok().json(CreateSecretResponseBody { id }))
        })
}

fn api_get_secret(
    path: web::Path<String>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let secret_id = path.into_inner();
    println!("Fetching: {:?}", &secret_id);
    state
        .consume_secret(&secret_id)
        .map(|res: Option<String>| match res {
            Some(secret) => HttpResponse::Ok().json(SecretBody { content: secret }),
            None => HttpResponse::NotFound().json(ApiError::from("not found")),
        })
}

fn view_secret_page(
    path: web::Path<String>,
    state: web::Data<AppState>,
) -> impl Future<Item = NamedFile, Error = actix_web::Error> {
    let secret_id = path.into_inner();

    state.fetch_secret(&secret_id).and_then(|res| match res {
        Some(_) => Ok(NamedFile::open("./static/view.html")?),

        None => {
            let file = NamedFile::open("./static/404.html")?
                // TODO: (requires HTTP crate).set_status_code()
                ;

            Ok(file)
        }
    })
}

fn config_from_env() -> ServerConfig {
    ServerConfig {
        redis_host: std::env::var("REDIS_HOST").unwrap_or("localhost:6379".to_string()),
        bind_host: std::env::var("BIND_HOST").unwrap_or("127.0.0.1:8080".to_string()),
        app_config: AppConfig {
            max_secret_length: std::env::var("MAX_SECRET_LENGTH")
                .unwrap_or("4096".to_string())
                .parse()
                .expect("unvalid integer value"),
            max_secret_expiration: std::env::var("MAX_SECRET_EXPIRATION")
                .unwrap_or("4096".to_string())
                .parse()
                .expect("unvalid integer value"),
            redis_key_prefix: std::env::var("REDIS_KEY_PREFIX")
                .unwrap_or("tofu:".to_string()),
        },
    }
}

fn main() -> io::Result<()> {
    env_logger::init();
    dotenv().ok();

    let config = config_from_env();
    println!("Booting with config: {:?}", config);

    let redis_addr = RedisActor::start(config.redis_host);
    let app_config = config.app_config;

    HttpServer::new(move || {
        let state = AppState {
            redis: redis_addr.clone(),
            config: app_config.clone(),
        };

        App::new()
            .data(state)
            .wrap(middleware::Logger::default())
            .service(
                web::resource("/api/secret")
                    .route(web::post().to_async(api_create_secret)),
            )
            .service(
                web::resource("/api/secret/{secret_id}")
                    .route(web::get().to_async(api_get_secret)),
            )
            .service(
                web::resource("/view/{secret_id}")
                    .route(web::get().to_async(view_secret_page)),
            )
            .service(Files::new("/", "./static/").index_file("index.html"))
    })
    .bind(config.bind_host)?
    .run()
}
