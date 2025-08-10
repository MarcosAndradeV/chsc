const TALLOC_BUF_CAP = 8000000;
var talloc_buf[TALLOC_BUF_CAP]char;
var talloc_buf_len size = 0;

fn talloc_restore(save_point size) {
    talloc_buf_len = save_point;
}
fn talloc_save() -> size {
    return talloc_buf_len;
}
fn talloc_reset() {
    talloc_buf_len = 0;
}
fn talloc(size size) -> *void {
    talloc_buf_len = talloc_buf_len + size;
    if(talloc_buf_len >= TALLOC_BUF_CAP) {
        talloc_buf_len = 0;
        return talloc_buf;
    }
    return talloc_buf + talloc_buf_len;
}
