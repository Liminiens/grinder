using Microsoft.EntityFrameworkCore.Migrations;

namespace Grinder.DataAccess.Migrations
{
    public partial class Initial : Migration
    {
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "AdminUsers",
                columns: table => new
                {
                    Username = table.Column<string>(nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_AdminUsers", x => x.Username);
                });

            migrationBuilder.CreateTable(
                name: "ChatsToMonitor",
                columns: table => new
                {
                    Username = table.Column<string>(nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_ChatsToMonitor", x => x.Username);
                });

            migrationBuilder.CreateTable(
                name: "Messages",
                columns: table => new
                {
                    Id = table.Column<long>(nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    MessageId = table.Column<long>(nullable: false),
                    ChatId = table.Column<long>(nullable: false),
                    UserId = table.Column<long>(nullable: false),
                    Date = table.Column<long>(nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Messages", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Users",
                columns: table => new
                {
                    Id = table.Column<long>(nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    UserId = table.Column<long>(nullable: false),
                    Username = table.Column<string>(nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Users", x => x.Id);
                });

            migrationBuilder.CreateIndex(
                name: "IX_AdminUsers_Username",
                table: "AdminUsers",
                column: "Username");

            migrationBuilder.CreateIndex(
                name: "IX_ChatsToMonitor_Username",
                table: "ChatsToMonitor",
                column: "Username");

            migrationBuilder.CreateIndex(
                name: "IX_Messages_Date",
                table: "Messages",
                column: "Date");

            migrationBuilder.CreateIndex(
                name: "IX_Messages_ChatId_UserId",
                table: "Messages",
                columns: new[] { "ChatId", "UserId" },
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Users_Username_UserId",
                table: "Users",
                columns: new[] { "Username", "UserId" },
                unique: true);
        }

        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "AdminUsers");

            migrationBuilder.DropTable(
                name: "ChatsToMonitor");

            migrationBuilder.DropTable(
                name: "Messages");

            migrationBuilder.DropTable(
                name: "Users");
        }
    }
}
