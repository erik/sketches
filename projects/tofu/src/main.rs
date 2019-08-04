#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;
extern crate dotenv;

use futures::future::Future;
use std::env;
use std::io;

use actix::prelude::Addr;
use actix_redis::{Command, RedisActor, RespValue};
use actix_web::{middleware, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use dotenv::dotenv;
use uuid::Uuid;

#[derive(Deserialize)]
pub struct CacheableSecret {
    a: String,
}

#[derive(Deserialize)]
pub struct CreateSecretPostBody {
    content: String,
    expiration_seconds: u32,
}

#[derive(Serialize)]
pub struct CreateSecretResponseBody {
    id: Uuid,
}

struct AppState {
    redis: Addr<RedisActor>,
}

fn create_secret(
    body: web::Json<CreateSecretPostBody>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let body = body.into_inner();
    let id = Uuid::new_v4();
    let expiration = body.expiration_seconds.to_string();
    println!("Writing: {:?}: {:?}", id.to_string(), expiration);

    state
        .redis
        .send(Command(resp_array![
            "SETEX",
            id.to_string(),
            expiration,
            body.content
        ]))
        .map_err(actix_web::Error::from)
        .map(move |_| HttpResponse::Ok().json(CreateSecretResponseBody { id }))
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
            Ok(RespValue::BulkString(vec)) => {
                let str = String::from_utf8(vec).unwrap();

                HttpResponse::Ok().body(str)
            }
            _ => HttpResponse::NotFound().body("not found"),
        })
}

fn main() -> io::Result<()> {
    env_logger::init();
    dotenv().ok();

    HttpServer::new(move || {
        let state = AppState {
            redis: RedisActor::start("localhost:6379"),
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
