#![allow(dead_code)]

use crate::libraries::runtime::http_helpers;
use serde_json::Value;

pub const FTPDB_BASE_URL: &str = "https://ftpdb.jam06452.uk";
pub const FTPDB_API_BASE: &str = "https://ftpdb.jam06452.uk/api";

pub mod helpers {
    use std::collections::BTreeMap;

    fn ret_i64(value: u32) -> Vec<u8> {
        vec![
            0x55,
            0x48,
            0x89,
            0xE5,
            0xB8,
            (value & 0xFF) as u8,
            ((value >> 8) & 0xFF) as u8,
            ((value >> 16) & 0xFF) as u8,
            ((value >> 24) & 0xFF) as u8,
            0x5D,
            0xC3,
        ]
    }

    pub fn get_ftpdb_helpers() -> BTreeMap<&'static str, Vec<u8>> {
        let mut m = BTreeMap::new();

        m.insert("ftpdb_hot", ret_i64(0));
        m.insert("ftpdb_top_this_week", ret_i64(0));
        m.insert("ftpdb_fan_favourites", ret_i64(0));
        m.insert("ftpdb_top_all_time", ret_i64(0));
        m.insert("ftpdb_most_time_spent", ret_i64(0));
        m.insert("ftpdb_random_projects", ret_i64(0));
        m.insert("ftpdb_random_devlogs", ret_i64(0));
        m.insert("ftpdb_devlogs", ret_i64(0));
        m.insert("ftpdb_project_info", ret_i64(0));
        m.insert("ftpdb_user_info", ret_i64(0));
        m.insert("ftpdb_user_projects", ret_i64(0));

        m
    }
}

pub fn base_url() -> &'static str {
    FTPDB_BASE_URL
}

pub fn api_base_url() -> &'static str {
    FTPDB_API_BASE
}

pub fn endpoint_hot() -> &'static str {
    "/hot"
}

pub fn endpoint_top_this_week() -> &'static str {
    "/top_this_week"
}

pub fn endpoint_fan_favourites() -> &'static str {
    "/fan_favourites"
}

pub fn endpoint_top_all_time() -> &'static str {
    "/top_all_time"
}

pub fn endpoint_most_time_spent() -> &'static str {
    "/most_time_spent"
}

pub fn endpoint_random_projects() -> &'static str {
    "/random_projects"
}

pub fn endpoint_random_devlogs() -> &'static str {
    "/random_devlogs"
}

pub fn endpoint_devlogs(project_id: &str) -> String {
    format!("/devlogs/{}", project_id)
}

pub fn endpoint_project_info(project_id: &str) -> String {
    format!("/project_info/{}", project_id)
}

pub fn endpoint_user_info(user_id: &str) -> String {
    format!("/user_info/{}", user_id)
}

pub fn endpoint_user_projects(user_id: &str) -> String {
    format!("/user_projects/{}", user_id)
}

fn endpoint_url(path: &str) -> String {
    http_helpers::build_url(FTPDB_API_BASE, path)
}

pub async fn get_hot() -> Result<Value, String> {
    http_helpers::ftpdb_hot().await
}

pub async fn get_top_this_week() -> Result<Value, String> {
    http_helpers::ftpdb_top_this_week().await
}

pub async fn get_fan_favourites() -> Result<Value, String> {
    http_helpers::ftpdb_fan_favourites().await
}

pub async fn get_top_all_time() -> Result<Value, String> {
    http_helpers::ftpdb_top_all_time().await
}

pub async fn get_most_time_spent() -> Result<Value, String> {
    http_helpers::ftpdb_most_time_spent().await
}

pub async fn get_random_projects(filter: Option<&str>) -> Result<Value, String> {
    http_helpers::ftpdb_random_projects(filter).await
}

pub async fn get_random_devlogs() -> Result<Value, String> {
    http_helpers::ftpdb_random_devlogs().await
}

pub async fn get_devlogs(project_id: &str) -> Result<Value, String> {
    http_helpers::ftpdb_devlogs(project_id).await
}

pub async fn get_project_info(project_id: &str) -> Result<Value, String> {
    http_helpers::ftpdb_project_info(project_id).await
}

pub async fn get_user_info(user_id: &str) -> Result<Value, String> {
    http_helpers::ftpdb_user_info(user_id).await
}

pub async fn get_user_projects(user_id: &str) -> Result<Value, String> {
    http_helpers::ftpdb_user_projects(user_id).await
}
