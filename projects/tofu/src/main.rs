#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;

use std::io;

use actix::prelude::Addr;
use actix_files::{Files, NamedFile};
use actix_redis::{Command, RedisActor, RespValue};
use actix_web::{middleware, web, App, HttpRequest, HttpResponse, HttpServer, Result};
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

#[derive(Debug, Clone, Copy)]
struct AppConfig {
    max_secret_length: usize,
    max_secret_expiration: usize,
}

struct AppState {
    redis: Addr<RedisActor>,
    config: AppConfig,
}

impl AppState {
    fn create_secret(
        self: &AppState,
        id: Uuid,
        body: String,
        expiration_seconds: usize,
    ) -> impl Future<Item = bool, Error = actix_web::Error> {
        self.redis
            .send(Command(resp_array![
                "SETEX",
                id.to_string(),
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
        id: String,
    ) -> impl Future<Item = Option<String>, Error = actix_web::Error> {
        self.redis
            .send(Command(resp_array!["GET", id]))
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
        id: String,
    ) -> impl Future<Item = Option<String>, Error = actix_web::Error> {
        let remove_secret = self
            .redis
            .send(Command(resp_array!["DEL", id.clone()]))
            .map_err(actix_web::Error::from);

        self.fetch_secret(id)
            .and_then(|result| remove_secret.map(|_| result))
    }
}

fn create_secret(
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
                .create_secret(id, body.content, body.expiration_seconds)
                .map(move |_| HttpResponse::Ok().json(CreateSecretResponseBody { id }))
        })
}

fn get_secret(
    path: web::Path<String>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let secret_id = path.to_string();
    println!("Fetching: {:?}", secret_id);

    state
        .consume_secret(secret_id)
        .map(|res: Option<String>| match res {
            Some(secret) => HttpResponse::Ok().json(SecretBody { content: secret }),
            None => HttpResponse::NotFound().json(ApiError::from("not found")),
        })
}

// fn view_secret_page(
//     path: web::Path<String>,
//     state: web::Data<AppState>,
// ) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
// }

fn main() -> io::Result<()> {
    env_logger::init();
    dotenv().ok();

    let config = AppConfig {
        max_secret_length: std::env::var("MAX_SECRET_LENGTH")
            .unwrap_or("4096".to_string())
            .parse()
            .expect("unvalid integer value"),
        max_secret_expiration: std::env::var("MAX_SECRET_EXPIRATION")
            .unwrap_or("4096".to_string())
            .parse()
            .expect("unvalid integer value"),
    };

    let bind_addr = std::env::var("BIND_ADDR").unwrap_or("127.0.0.1:8080".to_string());

    println!("Booting with config: {:?}", config);

    HttpServer::new(move || {
        let state = AppState {
            redis: RedisActor::start("localhost:6379"),
            config: config,
        };

        App::new()
            .data(state)
            .wrap(middleware::Logger::default())
            .service(
                web::resource("/api/secret").route(web::post().to_async(create_secret)),
            )
            .service(
                web::resource("/api/secret/{secret_id}")
                    .route(web::get().to_async(get_secret)),
            )
            .service(web::resource("/view/{secret_id}").route(web::get().to(
                |_: HttpRequest| -> Result<NamedFile> {
                    Ok(NamedFile::open("./static/view.html")?)
                },
            )))
            .service(Files::new("/", "./static/").index_file("index.html"))
    })
    .bind(bind_addr)?
    .run()
}
