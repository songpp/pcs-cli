pcs-cli
=======

CommandLine Baidu PCS

目前仅支持：

- 配额
- 搜索
- 下载
- 上传



Build & Install
-----

安装Haskell Platform 使用Cabal构建

	cabal sandbox init
	cabal install --only-dependencies
	cabal configure
	cabal build
	cabal install --prefix=$HOME/.cabal
	


使用
----

1. [注册一个百度APP](http://developer.baidu.com/console#app/project), 开启PCS API
2. 在当前的用户帐号根目录（unix: `~/`,  windows:`C:/Users/<your account name>/`）下创建文件`.pcs-cli.app.json`,格式如下：

		{
			"appKey" : "<app的API KEY>",
			"secretKey" : "<app的Secret Key>",
			"appPath" : "<app的PCS PATH, 比如： /app/cli >"
		}

3. 然后执行以下命令取得Token：`pcs-cli auth` 。AccessToken会保存在这个文件:`.pcs-cli.token.json`, 即token请求的http返回内容，可以通过其他工具获得就可以跳过这个token步骤。

		{
			"expires_in":2592000,
			"refresh_token":"27.f939971e26ddf46976a00085dcd4360b.2276084",
			"access_token":"26.a0356d7ba4a1ca9c4b6f17165ee8444f.2276084",
			"session_secret":"....................",
			"session_key":"....................",
			"scope":"basic super_msg netdisk"
		}

4. 使用`pcs-cli --help`查看都支持哪些命令， 也可以取得子命令的帮助：`pcs-cli search --help`


* 上传多个文件
		
		for z in $(find . -name '*.zip'); do pcs-cli upload -f $z -p /backup/ -o ; done


* 搜索
		
		pcs-cli search -r -k pdf

DEBUG
----
使用以下方式可以打印请求的url地址和参数信息

	env DEBUG=1 pcs-cli [command] [options]


TODO
----
....................
