#[macro_use]
extern crate redis_async;
#[macro_use]
extern crate serde_derive;
extern crate dotenv;

use futures::future::Future;
use std::env;
use std::io;

use actix::prelude::Addr;
use actix_redis::{Command, RedisActor};
use actix_web::{middleware, web, App, HttpResponse, HttpServer, Responder};
use dotenv::dotenv;

#[derive(Deserialize)]
pub struct CacheableSecret {
    a: String,
}

#[derive(Deserialize)]
pub struct CreateSecretPostBody {
    content: String,
    expiration_seconds: u32,
}

struct AppState {
    redis: Addr<RedisActor>,
}

fn create_secret(
    body: web::Json<CreateSecretPostBody>,
    state: web::Data<AppState>,
) -> impl Future<Item = HttpResponse, Error = actix_web::Error> {
    let body = body.into_inner();
    let id = "asdf";

    // TODO: expiration
    state
        .redis
        .send(Command(resp_array!["SET", id, body.content]))
        .map_err(actix_web::Error::from)
        .and_then(|_| Ok(HttpResponse::Ok().body("did the thing")))
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
    })
    .bind("127.0.0.1:8080")?
    .run()
}
