library(httr)
library(rvest)
library(stringr)
library(dplyr)

# ==============================
# 豆瓣小组配置
# ==============================

GROUP_URLS <- c(
  "https://www.douban.com/group/751504/",
  "https://www.douban.com/group/661193/",
  "https://www.douban.com/group/629678/",
  "https://www.douban.com/group/677194/",
  "https://www.douban.com/group/748452/",
  "https://www.douban.com/group/713320/"
)

# 关键词
KEYWORDS <- c(
  "🐮", "牛", "张凌赫", "凌赫", "zlh", "ZLH",
  "煮鱼", "逐玉", "褶", "过火", "秒火", "这一秒",
  "🐌", "众星", "众⭐️", "归鸾", "刺棠"
)

# ==============================
# 获取豆瓣小组帖子
# ==============================

get_group_posts <- function(group_url) {

  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/153.0.0.0 Safari/537.36"
  )

  response <- tryCatch(
    GET(
      group_url,
      user_agent(ua),
      timeout(30)
    ),
    error = function(e) NULL
  )

  if (is.null(response) || http_error(response)) {
    message("❌ 无法访问：", group_url)
    return(data.frame(
      post_id = character(),
      title = character(),
      link = character(),
      author = character(),
      time = character(),
      stringsAsFactors = FALSE
    ))
  }

  message("✅ 成功访问：", group_url)

  page <- tryCatch(
    read_html(response),
    error = function(e) NULL
  )

  if (is.null(page)) {
    message("❌ 页面解析失败")
    return(data.frame(
      post_id = character(),
      title = character(),
      link = character(),
      author = character(),
      time = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- page %>% html_nodes("#content .olt tr")

  if (length(rows) == 0) {
    message("⚠️ 没有找到帖子列表")
    return(data.frame(
      post_id = character(),
      title = character(),
      link = character(),
      author = character(),
      time = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- lapply(rows, function(r) {

    a <- r %>% html_node("td.title a")

    if (is.null(a)) {
      return(NULL)
    }

    title <- a %>%
      html_text() %>%
      str_trim()

    link <- html_attr(a, "href")

    post_id <- str_extract(
      link,
      "\\d+"
    )

    author_node <- r %>%
      html_node("td:nth-child(3) a")

    time_node <- r %>%
      html_node("td.time")

    author <- if (!is.null(author_node)) {
      html_text(author_node) %>% str_trim()
    } else {
      ""
    }

    time <- if (!is.null(time_node)) {
      html_text(time_node) %>% str_trim()
    } else {
      ""
    }

    data.frame(
      post_id = post_id,
      title = title,
      link = link,
      author = author,
      time = time,
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()

  out
}

# ==============================
# 获取全部小组帖子
# ==============================

get_all_posts <- function(urls) {

  all_df <- lapply(
    urls,
    get_group_posts
  ) %>%
    bind_rows()

  if (
    nrow(all_df) == 0 ||
    !"post_id" %in% colnames(all_df)
  ) {
    return(data.frame(
      post_id = character(),
      title = character(),
      link = character(),
      author = character(),
      time = character(),
      stringsAsFactors = FALSE
    ))
  }

  distinct(
    all_df,
    post_id,
    .keep_all = TRUE
  )
}

# ==============================
# 开始测试
# ==============================

message("======================================")
message("豆瓣监控云端测试")
message("======================================")

current <- get_all_posts(GROUP_URLS)

message("")
message("获取到帖子数量：", nrow(current))

if (nrow(current) > 0) {

  keyword_regex <- paste(
    sapply(KEYWORDS, str_escape),
    collapse = "|"
  )

  found <- current %>%
    filter(
      str_detect(
        title,
        keyword_regex
      )
    )

  message(
    "匹配关键词的帖子数量：",
    nrow(found)
  )

  if (nrow(found) > 0) {

    message("")
    message("🚨 找到匹配帖子：")
    message("--------------------------------------")

    for (i in seq_len(nrow(found))) {

      post <- found[i, ]

      message(
        "标题：",
        post$title
      )

      message(
        "链接：",
        post$link
      )

      message(
        "作者：",
        post$author
      )

      message(
        "时间：",
        post$time
      )

      message("--------------------------------------")
    }
  }

} else {

  message("⚠️ 没有获取到任何帖子")
}

message("")
message("测试结束。")
