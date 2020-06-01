using Microsoft.EntityFrameworkCore.Migrations;

namespace Grinder.DataAccess.Migrations
{
    public partial class MessageIndexes : Migration
    {
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropPrimaryKey(
                name: "PK_Message",
                table: "Message");

            migrationBuilder.DropIndex(
                name: "IX_Message_UserId",
                table: "Message");

            migrationBuilder.RenameTable(
                name: "Message",
                newName: "Messages");

            migrationBuilder.CreateIndex(
                name: "IX_Messages_Date",
                table: "Messages",
                column: "Date");

            migrationBuilder.CreateIndex(
                name: "IX_Messages_ChatId_UserId",
                table: "Messages",
                columns: new[] { "ChatId", "UserId" },
                unique: true);
        }

        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropIndex(
                name: "IX_Messages_Date",
                table: "Messages");

            migrationBuilder.DropIndex(
                name: "IX_Messages_ChatId_UserId",
                table: "Messages");

            migrationBuilder.RenameTable(
                name: "Messages",
                newName: "Message");

            migrationBuilder.AddPrimaryKey(
                name: "PK_Message",
                table: "Message",
                column: "MessageId");

            migrationBuilder.CreateIndex(
                name: "IX_Message_UserId",
                table: "Message",
                column: "UserId",
                unique: true);
        }
    }
}
