library(httr)
library(rvest)
library(stringr)
library(dplyr)

# ==========================================
# 豆瓣小组关键词监控 - GitHub Actions 云端版
# ==========================================

group_urls <- c(
  "https://www.douban.com/group/751504/",
  "https://www.douban.com/group/661193/",
  "https://www.douban.com/group/629678/",
  "https://www.douban.com/group/677194/",
  "https://www.douban.com/group/748452/",
  "https://www.douban.com/group/713320/"
)

keywords <- c(
  "🐮", "牛", "张凌赫", "凌赫", "zlh", "ZLH",
  "煮鱼", "逐玉", "褶", "过火", "秒火", "这一秒",
  "🐌", "众星", "众⭐️", "归鸾", "刺棠"
)

seen_file <- "seen_posts.txt"

cat("======================================\n")
cat("豆瓣监控云端版\n")
cat("======================================\n\n")

# ------------------------------------------
# 读取已经处理过的帖子 ID
# ------------------------------------------

if (file.exists(seen_file)) {
  seen_posts <- readLines(seen_file, warn = FALSE)
  seen_posts <- unique(seen_posts[nchar(seen_posts) > 0])
} else {
  seen_posts <- character(0)
}

cat(
  "历史已记录帖子数量：",
  length(seen_posts),
  "\n\n"
)

# ------------------------------------------
# 抓取单个小组
# ------------------------------------------

get_group_posts <- function(url) {

  tryCatch({

    response <- GET(
      url,
      add_headers(
        `User-Agent` =
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/153.0 Safari/537.36"
      ),
      timeout(30)
    )

    if (status_code(response) != 200) {

      cat(
        "❌ 访问失败：",
        url,
        " HTTP ",
        status_code(response),
        "\n"
      )

      return(data.frame())
    }

    cat(
      "✅ 成功访问：",
      url,
      "\n"
    )

    page <- read_html(
      content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    )

    rows <- page %>%
      html_elements("#content .olt tr")

    if (length(rows) == 0) {

      cat(
        "⚠️ 未解析到帖子：",
        url,
        "\n"
      )

      return(data.frame())
    }

    posts <- lapply(
      rows,
      function(row) {

        link_node <- row %>%
          html_element("td.title a")

        if (length(link_node) == 0) {
          return(NULL)
        }

        title <- html_text2(link_node)

        link <- html_attr(
          link_node,
          "href"
        )

        post_id <- str_extract(
          link,
          "(?<=/topic/)\\d+"
        )

        if (is.na(post_id)) {
          return(NULL)
        }

        author <- row %>%
          html_element("td.author") %>%
          html_text2()

        post_time <- row %>%
          html_element("td.time") %>%
          html_text2()

        data.frame(
          post_id = post_id,
          title = title,
          link = link,
          author = author,
          time = post_time,
          stringsAsFactors = FALSE
        )
      }
    )

    posts <- Filter(
      Negate(is.null),
      posts
    )

    if (length(posts) == 0) {
      return(data.frame())
    }

    bind_rows(posts)

  }, error = function(e) {

    cat(
      "❌ 抓取异常：",
      url,
      "\n"
    )

    cat(
      "错误信息：",
      conditionMessage(e),
      "\n"
    )

    return(data.frame())
  })
}

# ------------------------------------------
# 抓取全部小组
# ------------------------------------------

all_posts <- bind_rows(
  lapply(
    group_urls,
    get_group_posts
  )
)

cat("\n======================================\n")

cat(
  "本次获取帖子数量：",
  nrow(all_posts),
  "\n"
)

cat("======================================\n\n")

if (nrow(all_posts) == 0) {

  cat(
    "⚠️ 本次没有获取到帖子。\n"
  )

  quit(
    save = "no",
    status = 0
  )
}

# ------------------------------------------
# 只保留“新帖子”
# ------------------------------------------

new_posts <- all_posts %>%
  filter(
    !post_id %in% seen_posts
  ) %>%
  distinct(
    post_id,
    .keep_all = TRUE
  )

cat(
  "本次新帖子数量：",
  nrow(new_posts),
  "\n"
)

# ------------------------------------------
# 关键词匹配
# ------------------------------------------

if (nrow(new_posts) > 0) {

  pattern <- paste(
    str_replace_all(
      keywords,
      "([\\^$.|?*+(){}\\[\\]])",
      "\\\\\\1"
    ),
    collapse = "|"
  )

  matched_posts <- new_posts %>%
    filter(
      str_detect(
        title,
        regex(
          pattern,
          ignore_case = FALSE
        )
      )
    )

} else {

  matched_posts <- data.frame()
}

cat(
  "新帖子中匹配关键词数量：",
  nrow(matched_posts),
  "\n\n"
)

# ------------------------------------------
# 输出匹配结果 + 19 个 Server酱通知
# ------------------------------------------

if (nrow(matched_posts) > 0) {

  cat(
    "🚨 发现新的关键词帖子：\n\n"
  )

  for (i in seq_len(nrow(matched_posts))) {

    cat(
      "--------------------------------------\n"
    )

    cat(
      "标题：",
      matched_posts$title[i],
      "\n"
    )

    cat(
      "链接：",
      matched_posts$link[i],
      "\n"
    )

    cat(
      "作者：",
      matched_posts$author[i],
      "\n"
    )

    cat(
      "时间：",
      matched_posts$time[i],
      "\n"
    )

    cat(
      "--------------------------------------\n\n"
    )
  }

  # ----------------------------------------
  # 读取 19 个 Server酱 SendKey
  # ----------------------------------------

  sendkey_names <- c(
    "SERVERCHAN_SENDKEY",
    paste0(
      "SERVERCHAN_SENDKEY_",
      2:19
    )
  )

  sendkeys <- Sys.getenv(
    sendkey_names,
    unset = ""
  )

  valid_sendkeys <- sendkeys[
    nchar(
      trimws(sendkeys)
    ) > 0
  ]

  cat(
    "📱 已找到 ",
    length(valid_sendkeys),
    " 个 Server酱 SendKey\n",
    sep = ""
  )

  if (length(valid_sendkeys) == 0) {

    cat(
      "⚠️ 没有找到任何 Server酱 SendKey，跳过通知。\n"
    )

  } else {

    cat(
      "📨 开始发送 Server酱通知...\n\n"
    )

    # --------------------------------------
    # 对每一条匹配帖子发送通知
    # --------------------------------------

    for (i in seq_len(nrow(matched_posts))) {

      # ------------------------------------
      # Server酱顶部标题
      # ------------------------------------

      title <- paste0(
        "db发现匹配关键词的帖子！注意发帖时间哦"
      )

      # ------------------------------------
      # Server酱正文
      # ------------------------------------

      desp <- paste0(
        "**标题：** ",
        matched_posts$title[i],
        "\n\n",
        "**作者：** ",
        matched_posts$author[i],
        "\n\n",
        "**时间：** ",
        matched_posts$time[i],
        "\n\n",
        "**链接：** ",
        matched_posts$link[i]
      )

      # --------------------------------------
      # 依次发送给全部 SendKey
      # --------------------------------------

      for (j in seq_along(valid_sendkeys)) {

        api_url <- paste0(
          "https://sctapi.ftqq.com/",
          valid_sendkeys[j],
          ".send"
        )

        result <- tryCatch({

          response <- POST(
            api_url,
            body = list(
              title = title,
              desp = desp
            ),
            encode = "form",
            timeout(30)
          )

          content(
            response,
            as = "text",
            encoding = "UTF-8"
          )

        }, error = function(e) {

          paste0(
            "发送异常：",
            conditionMessage(e)
          )
        })

        cat(
          "SendKey ",
          j,
          "/",
          length(valid_sendkeys),
          "：",
          result,
          "\n",
          sep = ""
        )
      }
    }

    cat(
      "\n✅ 全部 Server酱通知发送完成。\n"
    )
  }

} else {

  cat(
    "本次没有新的关键词帖子。\n"
  )
}

# ------------------------------------------
# 更新历史记录
# ------------------------------------------

all_seen <- unique(
  c(
    seen_posts,
    all_posts$post_id
  )
)

writeLines(
  all_seen,
  seen_file
)

cat(
  "历史记录已更新：",
  length(all_seen),
  " 个帖子 ID\n"
)

cat(
  "\n测试结束。\n"
)
