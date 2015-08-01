..\packages\OpenCover.4.6.166\tools\OpenCover.Console.exe -target:"bin\Debug\MiniJson.Tests.exe" -filter:"+[MiniJson]*"
..\packages\ReportGenerator.2.1.8.0\tools\ReportGenerator.exe -reports:results.xml -targetdir:bin\Debug\coverage
.\bin\Debug\coverage\index.htm
