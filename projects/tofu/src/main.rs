#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;
extern crate dotenv;

use std::io;

use actix::prelude::Addr;
use actix_redis::{Command, RedisActor, RespValue};
use actix_web::{middleware, web, App, HttpResponse, HttpServer};
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

fn create_secret(
    body: web::Json<CreateSecretPostBody>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let body = body.into_inner();

    let id = Uuid::new_v4();
    let expiration = body.expiration_seconds.to_string();

    let validation = future::result(if body.content.len() > state.config.max_secret_length {
        Err("content too large")
    } else if body.expiration_seconds > state.config.max_secret_expiration {
        Err("expiration too long")
    } else {
        Ok(())
    })
    .map_err(|err| HttpResponse::BadRequest().json(ApiError::from(err)))
    .map_err(actix_web::Error::from);

    println!("Writing: {:?}: {:?}", id.to_string(), expiration);

    let write_secret = state
        .redis
        .send(Command(resp_array![
            "SETEX",
            id.to_string(),
            expiration,
            body.content
        ]))
        .map_err(actix_web::Error::from)
        .map(move |_| HttpResponse::Ok().json(CreateSecretResponseBody { id }));

    validation.and_then(|_| write_secret)
}

fn get_secret(
    path: web::Path<String>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let secret_id = path.to_string();
    println!("Fetching: {:?}", secret_id);

    let fetch_secret = state
        .redis
        .send(Command(resp_array!["GET", secret_id.clone()]))
        .map_err(actix_web::Error::from);

    let expire_secret = state
        .redis
        .send(Command(resp_array!["DEL", secret_id]))
        .map_err(actix_web::Error::from);

    fetch_secret
        .and_then(|result| expire_secret.map(|_| result))
        .map(|res: Result<RespValue, actix_redis::Error>| match res {
            Ok(RespValue::BulkString(bytes)) => {
                let str = String::from_utf8(bytes).expect("non-utf8 string in redis");
                HttpResponse::Ok().json(SecretBody { content: str })
            }

            Ok(RespValue::Nil) => HttpResponse::NotFound().json(ApiError::from("not found")),

            err => {
                println!("Redis exception: {:?}", err);
                HttpResponse::InternalServerError().json(ApiError::from("something went wrong"))
            }
        })
}

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

    println!("Booting with config: {:?}", config);

    HttpServer::new(move || {
        let state = AppState {
            redis: RedisActor::start("localhost:6379"),
            config: config,
        };

        App::new()
            .data(state)
            .wrap(middleware::Logger::default())
            .service(web::resource("/secret").route(web::post().to_async(create_secret)))
            .service(web::resource("/secret/{secret_id}").route(web::get().to_async(get_secret)))
    })
    .bind("127.0.0.1:8080")?
    .run()
}
