import vibe.d;
import std.socket;
import std.uuid;
import std.file;

string destPathRoot;

shared static this() {
  destPathRoot = readRequiredOption!string("p|path", "Path to file");

  Socket s = new TcpSocket();
  s.connect(new InternetAddress("google.com", 80));
  ushort openPort = to!ushort(s.localAddress.toPortString());
  string localAddr = s.localAddress.toAddrString();
  s.close();

	auto settings = new HTTPServerSettings;
	settings.port = openPort;
	settings.bindAddresses = ["::1", "0.0.0.0"];

  auto uuid = randomUUID();
  auto router = new URLRouter;
  router.get("/" ~ uuid.toString(), staticTemplate!"index.dt");
  router.post("/" ~ uuid.toString(), &recvFile);
	listenHTTP(settings, router);

	logInfo("Please open http://%s:%d/%s in your browser.", localAddr, openPort, uuid);
}

void recvFile(HTTPServerRequest req, HTTPServerResponse res) {
  auto pf = "file" in req.files;
  enforce(pf !is null, "No file uploaded!");

  Path filepath;
  if (exists(destPathRoot) && destPathRoot.isDir)
    filepath = Path(destPathRoot) ~ pf.filename;
  else
    filepath = Path(destPathRoot);

  try
    moveFile(pf.tempPath, filepath);
  catch (Exception e)
    copyFile(pf.tempPath, filepath); 

  res.writeBody("Success!");

  yield();
  exitEventLoop();
}
