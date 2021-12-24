use assert_cmd::Command;
use std::fs::{read_to_string, File};
use std::io::Write;
use tempdir::TempDir;

fn create_test_files<'a>(files: impl IntoIterator<Item = &'a (&'a str, &'a str)>) -> TempDir {
    let test_dir = TempDir::new("fnr-integration-test").unwrap();
    for (name, contents) in files {
        let path = test_dir.path().join(name);
        let mut file = File::create(path).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }
    test_dir
}

#[test]
fn test_simple_replace_without_write() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content), ("bar.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&["foo", "bar", test_dir.path().to_str().unwrap()])
        .assert()
        .success();

    for file in &["foo.txt", "bar.txt"] {
        let contents = read_to_string(test_dir.path().join(file)).unwrap();
        assert_eq!(contents, orig_content);
    }
}

#[test]
fn test_simple_replace_with_write() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content), ("bar.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&["foo", "bar", "--write", test_dir.path().to_str().unwrap()])
        .assert()
        .success();

    let expected_content = "bar\nbar\nbaz\n";

    for file in &["foo.txt", "bar.txt"] {
        let contents = read_to_string(test_dir.path().join(file)).unwrap();
        assert_eq!(contents, expected_content);
    }
}

#[test]
fn test_simple_replace_with_file_path() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content), ("bar.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "foo",
            "bar",
            "--write",
            test_dir.path().join("foo.txt").to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "bar\nbar\nbaz\n";

    assert_eq!(
        read_to_string(test_dir.path().join("bar.txt")).unwrap(),
        orig_content
    );
    assert_eq!(
        read_to_string(test_dir.path().join("foo.txt")).unwrap(),
        modified_content
    );
}

#[test]
fn test_replace_with_capture_groups() {
    let orig_content = "abc\n123\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "([a-z])[a-z]([a-z])",
            "$1$2$1$$",
            "--write",
            test_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let expected_content = "aca$\n123\n";
    let contents = read_to_string(test_dir.path().join("foo.txt")).unwrap();
    assert_eq!(contents, expected_content);
}

#[test]
fn test_simple_replace_smart_case_lower() {
    let orig_content = "FoO\nfoo\nBaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "foo",
            "bar",
            // -S --smart-case should be the default
            "--write",
            test_dir.path().join("foo.txt").to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "bar\nbar\nBaz\n";
    assert_eq!(
        read_to_string(test_dir.path().join("foo.txt")).unwrap(),
        modified_content
    );
}

#[test]
fn test_simple_replace_smart_case_upper() {
    let orig_content = "FoO\nfoo\nBaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "FoO",
            "bar",
            // -S --smart-case should be the default
            "--write",
            test_dir.path().join("foo.txt").to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "bar\nfoo\nBaz\n";
    assert_eq!(
        read_to_string(test_dir.path().join("foo.txt")).unwrap(),
        modified_content
    );
}

#[test]
fn test_simple_replace_case_sensitive() {
    let orig_content = "FoO\nfoo\nBaz\n";
    let test_dir = create_test_files(&[("foo.txt", orig_content)]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "foo",
            "bar",
            "--case-sensitive",
            "--write",
            test_dir.path().join("foo.txt").to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "FoO\nbar\nBaz\n";
    assert_eq!(
        read_to_string(test_dir.path().join("foo.txt")).unwrap(),
        modified_content
    );
}

#[test]
fn test_simple_replace_ignore_hidden() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[
        (".hidden_file", orig_content),
        ("vcs-hidden", orig_content),
        (".ignore", "vcs-hidden"),
    ]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&["foo", "bar", "--write", test_dir.path().to_str().unwrap()])
        .assert()
        .success();

    assert_eq!(
        read_to_string(test_dir.path().join(".hidden_file")).unwrap(),
        orig_content
    );
    assert_eq!(
        read_to_string(test_dir.path().join("vcs-hidden")).unwrap(),
        orig_content
    );
}

#[test]
fn test_simple_replace_include_hidden() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[
        (".hidden_file", orig_content),
        ("vcs-hidden", orig_content),
        (".ignore", "vcs-hidden"),
    ]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "foo",
            "bar",
            "--hidden",
            "--write",
            test_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "bar\nbar\nbaz\n";
    assert_eq!(
        read_to_string(test_dir.path().join(".hidden_file")).unwrap(),
        modified_content
    );
    assert_eq!(
        read_to_string(test_dir.path().join("vcs-hidden")).unwrap(),
        orig_content
    );
}

#[test]
fn test_simple_replace_include_all() {
    let orig_content = "foo\nbar\nbaz\n";
    let test_dir = create_test_files(&[
        (".hidden_file", orig_content),
        ("vcs-hidden", orig_content),
        (".ignore", "vcs-hidden"),
    ]);

    Command::cargo_bin("fnr")
        .unwrap()
        .args(&[
            "foo",
            "bar",
            "--all-files",
            "--write",
            test_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let modified_content = "bar\nbar\nbaz\n";
    assert_eq!(
        read_to_string(test_dir.path().join(".hidden_file")).unwrap(),
        modified_content
    );
    assert_eq!(
        read_to_string(test_dir.path().join("vcs-hidden")).unwrap(),
        modified_content
    );
}
