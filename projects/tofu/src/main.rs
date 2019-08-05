#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;
extern crate dotenv;

use futures::future;
use futures::future::Future;
use std::io;

use actix::prelude::Addr;
use actix_redis::{Command, RedisActor, RespValue};
use actix_web::{middleware, web, App, HttpResponse, HttpServer};
use dotenv::dotenv;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
pub struct CacheableSecret {
    content: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct CreateSecretPostBody {
    content: String,
    expiration_seconds: u32,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct CreateSecretResponseBody {
    id: Uuid,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum GetSecretResponse {
    Success(CacheableSecret),
    Error(String),
}

struct AppState {
    redis: Addr<RedisActor>,
    max_secret_length: usize,
}

fn create_secret(
    body: web::Json<CreateSecretPostBody>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let body = body.into_inner();

    let id = Uuid::new_v4();
    let expiration = body.expiration_seconds.to_string();

    let validation = future::result(if body.content.len() > state.max_secret_length {
        Err("content too large")
    } else if body.expiration_seconds > 9000 {
        Err("expiration too long")
    } else {
        Ok(())
    })
    .map_err(actix_web::error::ErrorBadRequest);

    println!("Writing: {:?}: {:?}", id.to_string(), expiration);

    let redis_fetch = state
        .redis
        .send(Command(resp_array![
            "SETEX",
            id.to_string(),
            expiration,
            body.content
        ]))
        .map_err(actix_web::Error::from)
        .map(move |_| HttpResponse::Ok().json(CreateSecretResponseBody { id }));

    validation.and_then(|_| redis_fetch)
}

fn get_secret(
    path: web::Path<String>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let secret_id = path.to_string();
    println!("Fetching: {:?}", secret_id);

    state
        .redis
        .send(Command(resp_array!["GET", secret_id]))
        .map_err(actix_web::Error::from)
        .map(|res: Result<RespValue, actix_redis::Error>| match res {
            Ok(RespValue::BulkString(bytes)) => {
                let str = String::from_utf8(bytes).expect("non-utf8 string in redis");
                HttpResponse::Ok()
                    .json(GetSecretResponse::Success(CacheableSecret { content: str }))
            }
            _ => HttpResponse::NotFound().json(GetSecretResponse::Error("not found".to_string())),
        })
}

fn main() -> io::Result<()> {
    env_logger::init();
    dotenv().ok();

    HttpServer::new(move || {
        let state = AppState {
            redis: RedisActor::start("localhost:6379"),
            max_secret_length: std::env::var("MAX_SECRET_LENGTH")
                .unwrap_or("4096".to_string())
                .parse()
                .expect("unvalid integer value"),
        };

        App::new()
            .data(state)
            .wrap(middleware::Logger::default())
            .service(web::resource("/secret").route(web::post().to_async(create_secret)))
            .service(web::resource("/secret/{secretid}").route(web::get().to_async(get_secret)))
    })
    .bind("127.0.0.1:8080")?
    .run()
}
